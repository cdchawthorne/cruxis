import os
import re
import shutil
import stat
import subprocess

import cruxis.exceptions
import cruxis.network

class _WpaConfFile:
    '''
    Abstraction of a wpa_supplicant.conf file.
    Can be an actual file or just a holder for an ssid and a key.
    Any of the filename, ssid, and key is allowed to be unsanitized; any
    operations within this class are safe no matter the input data.
    See specific methods for whether the return values are sanitized.
    '''
    def __init__(self, *args, **kwargs):
        if len(args) + len(kwargs) == 2:
            self.__init_from_data(*args, **kwargs)
        elif len(args) + len(kwargs) == 1:
            self.__init_from_filename(*args, **kwargs)
        else:
            raise TypeError("Incorrect number of arguments")

    def __init_from_data(self, ssid, key):
        self.__ssid = ssid
        self.__key = key
        self.__filename = None

    def __init_from_filename(self, filename):
        self.__ssid = None
        self.__key = None
        self.__filename = filename

    @property
    def ssid(self):
        '''Return the unsanitized ssid'''
        if self.__filename is not None:
            with open(self.__filename, "r") as f:
                for line in f:
                    ssid_match = re.search(r'^\s*ssid="?([^"]*)"?$', line)
                    if ssid_match:
                        return ssid_match.expand(r'\1')

            raise cruxis.exceptions.BadWpaConfFileError(self.__filename)
        else:
            return self.__ssid

    def write_to_filename(self, dst):
        '''
        Make a copy of this _WpaConfFile in the filesystem.
        The copy will have file mode 600.
        '''
        if self.__filename is not None:
            shutil.copyfile(self.__filename, dst)
        else:
            with open(dst, "w") as fdst:
                proc = subprocess.Popen(['wpa_passphrase', self.__ssid],
                                        stdin=subprocess.PIPE, stdout=fdst)
                proc.communicate(self.__key.encode())
                ret = proc.wait()
                if ret:
                    raise cruxis.exceptions.NetworkStorageError()

        os.chmod(dst, stat.S_IRUSR | stat.S_IWUSR)

    def get_filename(self, alternate_filename):
        '''
        Return a filename to a file containing this _WpaConfFile.
        If self is already in the filesystem, return that; else, write the
        data to alternate_filename and return alternate_filename.
        '''
        if self.__filename is not None:
            return self.__filename
        else:
            self.write_to_filename(alternate_filename)
            return alternate_filename


class WpaNetwork(cruxis.network.Network):
    NAME = 'wpa'
    INFO_FILES = ("ssid", "wpa_supplicant.conf")
    DEFAULT_CONF_FILE = os.path.join(cruxis.network.Network.CRUXIS_DIR,
                                     "wpa_supplicant.conf")

    def __init__(self, *args, **kwargs):
        '''
        __init__(self, filename)
        __init__(self, ssid, key)
        '''
        self.__conf_file = _WpaConfFile(*args, **kwargs)
        self.__ssid = self.__conf_file.ssid
        self.__supplicant = None
        self.__remove_default_conf = False

    @property
    def ssid(self):
        return self.__ssid

    @classmethod
    def _get_by_path(cls, network_path):
        conf_file = os.path.join(network_path, "wpa_supplicant.conf")

        return cls(conf_file)

    def _specific_connect(self):
        subprocess.check_call(["ip", "link", "set", self.INTERFACE, "up"])
        subprocess.check_call(
                ["iwconfig", self.INTERFACE, "essid", self.__ssid])

        conf_filename = self.__conf_file.get_filename(self.DEFAULT_CONF_FILE)
        supplicant = subprocess.Popen(["wpa_supplicant", "-Dwext",
                                       "-i", self.INTERFACE,
                                       "-c", conf_filename])

        try:
            subprocess.check_call(["dhcpcd", self.INTERFACE])
        except subprocess.CalledProcessError as e:
            supplicant.terminate()
            supplicant.wait()
            subprocess.call(["ip", "link", "set", self.INTERFACE, "down"])
            raise cruxis.exceptions.ConnectionError(self.__ssid) from e

        self.__supplicant = supplicant
        self.__remove_default_conf = (self.DEFAULT_CONF_FILE == conf_filename)

    def _specific_disconnect(self):
        subprocess.check_call(["dhcpcd", "-x", self.INTERFACE])
        self.__supplicant.terminate()
        self.__supplicant.wait()
        self.__supplicant = None
        subprocess.check_call(["ip", "link", "set", self.INTERFACE, "down"])

        if self.__remove_default_conf:
            os.remove(self.DEFAULT_CONF_FILE)

        self.__remove_default_conf = False

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")

        self.__conf_file.write_to_filename(
                os.path.join(path, "wpa_supplicant.conf"))
