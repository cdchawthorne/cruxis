import abc
import os
import re
import shutil
import subprocess

import cruxis.exceptions

# TODO: should this track the currently connected network?
class Connector(metaclass=abc.ABCMeta):
    CRUXIS_DIR = '/etc/cruxis'
    TEST_URL = 'xkcd.com'

    __network_types = {}

    @classmethod
    def register_network_type(cls, network_type):
        assert hasattr(network_type, 'NAME')
        name = network_type.NAME
        assert name not in cls.__network_types
        cls.__network_types[name] = network_type

    @staticmethod
    def scan(currently_connected):
        if not currently_connected:
            subprocess.check_call(["ip", "link", "set", "wlan0", "up"])

        scan_output = subprocess.check_output(["iwlist", "wlan0", "scan"])

        if not currently_connected:
            subprocess.check_call(["ip", "link", "set", "wlan0", "down"])

        return scan_output

    @classmethod
    def create_network(cls, type_name, *args, **kwargs):
        try:
            network_type = cls.__network_types[type_name]
        except KeyError as e:
            raise UnknownNetworkTypeError(name)

        return network_type(*args, **kwargs)

    @classmethod
    def disconnect(cls):
        subprocess.call(["dhcpcd", "-x", "wlan0"])
        subprocess.call(["pkill", "wpa_supplicant"])
        subprocess.check_call(["ip", "link", "set", "wlan0", "down"])
        try:
            os.remove(os.path.join(cls.CRUXIS_DIR,
                                   "wpa_supplicant.conf"))
        except FileNotFoundError:
            pass

    @classmethod
    def check_connected(cls):
        ret = subprocess.call(["ping", "-c3", "-Iwlan0", cls.TEST_URL])
        return ret == 0

    def connect(self):
        self.disconnect()
        self._specific_connect()

    @staticmethod
    def scan_ssids():
        subprocess.check_call(["ip", "link", "set", "wlan0", "up"])
        scan_output = subprocess.check_output(["iwlist", "wlan0", "scan"])
        subprocess.check_call(["ip", "link", "set", "wlan0", "down"])
        ssids = []
        for line in scan_output.splitlines():
            ssid_match = re.search(rb'^\s*ESSID:"(.*)"$', line)
            if ssid_match:
                ssids.append(ssid_match.expand(rb'\1').decode())

        return ssids

    @abc.abstractproperty
    def ssid(self):
        pass

    @abc.abstractmethod
    def _specific_connect(self):
        pass
