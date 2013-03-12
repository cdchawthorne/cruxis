import os
import subprocess

import cruxis.network

class WepNetwork(cruxis.network.Network):
    NAME = 'wep'
    INFO_FILES = ("key", "ssid")

    def __init__(self, ssid, key):
        self.__ssid = ssid
        self.__key = key

    @classmethod
    def _get_by_path(cls, network_path):
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
            raise cruxis.exceptions.BadKeyError(self.__ssid)

        try:
            subprocess.check_call(["dhcpcd", "wlan0"])
        except subprocess.CalledProcessError as e:
            subprocess.call(["ip", "link", "set", "wlan0", "down"])
            raise cruxis.exceptions.ConnectionError(self.__ssid) from e

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")

        with open(os.path.join(path, "key"), "w") as f:
            f.write(self.__key + "\n")

        os.chmod(os.path.join(path, "key"), stat.S_IRUSR | stat.S_IWUSR)

