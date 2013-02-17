import abc
import contextlib
import io
import os
import re
import shlex
import shutil
import stat
import subprocess
import tempfile

import exceptions

# Bigger TODO: handle exceptions better
# TODO: abstract out shell functions?
class NetworkFile:
    def __init__(self, filename):
        self.__filename = filename

    @property
    def remembered_ids(self):
        with open(self.__filename, "r") as f:
            return [int(line.rstrip()) for line in f]

    def remember_network(self, network_id):
        network_id = str(network_id)
        with open(self.__filename, "r") as f:
            network_ids = [line.rstrip() for line in f]

        if network_id not in network_ids:
            with open(self.__filename, "a") as f:
                f.write(network_id + "\n")

    def forget_network(self, network_id):
        with open(self.__filename, "r") as f:
            remembered_ids = [line.rstrip() for line in f]

        with open(self.__filename, "w") as f:
            for remembered_id in remembered_ids:
                if remembered_id != str(network_id):
                    f.write(remembered_id + "\n")

class Network(metaclass=abc.ABCMeta):
    CRUXIS_DIR = '/etc/cruxis'
    NETWORKS_DIR = os.path.join(CRUXIS_DIR, 'networks.d')

    @classmethod
    def get_by_id(cls, network_id):
        network_factories = {
                ("ssid", "wpa_supplicant.conf"): WpaNetwork._create_from_id,
                ("key", "ssid"): WepNetwork._create_from_id,
                ("ssid",): UnsecuredNetwork._create_from_id,
                }

        network_path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(network_path):
            raise exceptions.NetworkNotFoundError(network_id)

        info_files = os.listdir(network_path)
        info_files.sort()
        network_factory = network_factories[tuple(info_files)]
        return network_factory(network_id)

    @classmethod
    def used_ids(cls):
        ids = [int(id_str) for id_str in os.listdir(cls.NETWORKS_DIR)]
        ids.sort()
        return ids

    @classmethod
    def is_id_used(cls, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        return os.path.exists(path)

    @classmethod
    def next_unused_id(cls):
        used_ids = [int(id_str) for id_str in os.listdir(cls.NETWORKS_DIR)]
        i = 0
        while i in used_ids:
            i = i + 1

        return i

    @classmethod
    def disconnect(cls):
        subprocess.call(["pkill", "(wpa_supplicant|dhcpcd)"])
        subprocess.check_call(["ip", "link", "set", "wlan0", "down"])
        try:
            os.remove(os.path.join(cls.NETWORKS_DIR,
                                   "wpa_supplicant.conf"))
        except FileNotFoundError:
            pass

    def connect(self):
        self.disconnect()
        self._specific_connect()

    @staticmethod
    def scan_ssids():
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        scan_output = subprocess.check_output(["iwlist", "wlan0", "scan"])
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        ssids = []
        for line in scan_output.splitlines():
            ssid_match = re.search(rb'^\s*ESSID:"(.*)"$', line)
            if ssid_match:
                ssids.append(ssid_match.expand(rb'\1').decode())

        return ssids

    @abc.abstractproperty
    def ssid(self):
        pass

    @classmethod
    @abc.abstractmethod
    def _create_from_id(cls, network_id):
        pass

    @abc.abstractmethod
    def _specific_connect(self):
        pass

    def write_to_fs(self, network_id):
        path = os.path.join(self.NETWORKS_DIR, str(network_id))
        if os.path.exists(path):
            raise exceptions.NetworkIdInUseError(network_id)

        os.mkdir(path)

        self._write_to_path(path)

    @classmethod
    def remove_from_fs(cls, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(path):
            raise exceptions.NetworkNotFoundError(network_id)

        shutil.rmtree(path)

    @abc.abstractmethod
    def _write_to_path(self, path):
        pass


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

            raise exceptions.BadWpaConfFileError(self.__filename)
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
                proc.wait()

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


class WpaNetwork(Network):

    def __init__(self, *args, **kwargs):
        '''
        __init__(self, filename)
        __init__(self, ssid, key)
        '''
        self.__conf_file = _WpaConfFile(*args, **kwargs)
        self.__ssid = self.__conf_file.ssid

    @property
    def ssid(self):
        return self.__ssid

    @classmethod
    def _create_from_id(cls, network_id):
        conf_file = os.path.join(cls.NETWORKS_DIR,
                                 str(network_id),
                                 "wpa_supplicant.conf")

        return cls(conf_file)

    def _specific_connect(self):
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        subprocess.check_call(["iwconfig", "wlan0", "essid", self.__ssid])

        default_conf_path = os.path.join(self.CRUXIS_DIR,
                                         'wpa_supplicant.conf')
        conf_filename = self.__conf_file.get_filename(default_conf_path)
        supplicant = subprocess.Popen(["wpa_supplicant", "-Dwext", "-iwlan0",
                                       "-c", conf_filename])

        try:
            subprocess.check_call(["dhcpcd", "wlan0"])
        except subprocess.CalledProcessError as e:
            supplicant.terminate()
            subprocess.call(["ip", "link", "set", "wlan0", "down"])
            raise exceptions.ConnectionError(self.__ssid) from e

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")

        self.__conf_file.write_to_filename(
                os.path.join(path, "wpa_supplicant.conf"))


class WepNetwork(Network):

    def __init__(self, ssid, key):
        self.__ssid = ssid
        self.__key = key

    @classmethod
    def _create_from_id(cls, network_id):
        network_path = os.path.join(cls.NETWORKS_DIR,
                                    str(network_id))

        with open(os.path.join(network_path, "ssid"), "r") as f:
            ssid = f.readline().rstrip()

        with open(os.path.join(network_path, "key"), "r") as f:
            key = f.readline().rstrip()

        return cls(ssid, key)

    @property
    def ssid(self):
        return self.__ssid

    def __formatted_key(self):
        try:
            int(self.__key, 16)
        except ValueError:
            return 's:' + self.__key
        else:
            return self.__key

    def _specific_connect(self):
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        try:
            subprocess.check_call(["iwconfig", "wlan0", "essid", self.__ssid,
                                   "key", self.__formatted_key()])
        except subprocess.CalledProcessError as e:
            raise exceptions.BadKeyError(self.__ssid)

        try:
            subprocess.check_call(["dhcpcd", "wlan0"])
        except subprocess.CalledProcessError as e:
            subprocess.call(["ip", "link", "set", "wlan0", "down"])
            raise exceptions.ConnectionError(self.__ssid) from e

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")

        with open(os.path.join(path, "key"), "w") as f:
            f.write(self.__key + "\n")

        os.chmod(os.path.join(path, "key"), stat.S_IRUSR | stat.S_IWUSR)


class UnsecuredNetwork(Network):

    def __init__(self, ssid):
        self.__ssid = ssid

    @classmethod
    def _create_from_id(cls, network_id):
        network_path = os.path.join(cls.NETWORKS_DIR,
                                    str(network_id))

        with open(os.path.join(network_path, "ssid"), "r") as f:
            ssid = f.readline().rstrip()

        return cls(ssid)

    @property
    def ssid(self):
        return self.__ssid

    def _specific_connect(self):
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        subprocess.check_call(["iwconfig", "wlan0", "essid", self.__ssid])

        try:
            subprocess.check_call(["dhcpcd", "wlan0"])
        except subprocess.CalledProcessError as e:
            subprocess.call(["ip", "link", "set", "wlan0", "down"])
            raise exceptions.ConnectionError(self.__ssid) from e

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")
