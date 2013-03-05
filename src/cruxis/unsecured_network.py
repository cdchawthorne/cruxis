import os
import subprocess

import cruxis.exceptions
import cruxis.network

@cruxis.network.network_type
class UnsecuredNetwork(cruxis.network.Network):
    NAME = 'unsecured'
    INFO_FILES = ("ssid",)

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
            raise cruxis.exceptions.ConnectionError(self.__ssid) from e

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")
