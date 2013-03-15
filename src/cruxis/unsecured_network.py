import os
import subprocess

import cruxis.exceptions
import cruxis.network

class UnsecuredNetwork(cruxis.network.Network):

    NAME = 'unsecured'
    INFO_FILES = ("ssid",)

    def __init__(self, ssid):
        self.__ssid = ssid

    @classmethod
    def _get_by_path(cls, network_path):
        with open(os.path.join(network_path, "ssid"), "r") as f:
            ssid = f.readline().rstrip()

        return cls(ssid)

    @property
    def ssid(self):
        return self.__ssid

    def _specific_connect(self):
        subprocess.check_call(["ip", "link", "set", self.INTERFACE, "up"])
        subprocess.check_call(
                ["iwconfig", self.INTERFACE, "essid", self.__ssid])

        try:
            subprocess.check_call(["dhcpcd", self.INTERFACE])
        except subprocess.CalledProcessError as e:
            subprocess.call(["ip", "link", "set", self.INTERFACE, "down"])
            raise cruxis.exceptions.ConnectionError(self.__ssid) from e

    def _specific_disconnect(self):
        subprocess.check_call(["dhcpcd", "-x", self.INTERFACE])
        subprocess.check_call(["ip", "link", "set", self.INTERFACE, "down"])

    def _write_to_path(self, path):
        with open(os.path.join(path, "ssid"), "w") as f:
            f.write(self.__ssid + "\n")
