import abc
import os
import re
import shutil
import subprocess

import cruxis.exceptions

#TODO: replace this with _NetworkMetaclass
def network_type(cls):
    Network._register_network(cls)

# TODO: should this track the currently connected network?
class Network(metaclass=abc.ABCMeta):
    CRUXIS_DIR = '/etc/cruxis'
    NETWORKS_DIR = os.path.join(CRUXIS_DIR, 'networks.d')
    TEST_URL = 'xkcd.com'

    __id_network_factories = {}
    __data_network_factories = {}

    @classmethod
    def _register_network(cls, network_type):
        info_files = network_type.INFO_FILES
        name = network_type.NAME
        assert info_files not in cls.__id_network_factories
        assert name not in cls.__data_network_factories
        cls.__id_network_factories[info_files] = network_type._create_from_id
        cls.__data_network_factories[name] = network_type

    @staticmethod
    def scan(currently_connected):
        if not currently_connected:
            subprocess.check_call(["ip", "link", "set", "wlan0", "up"])

        scan_output = subprocess.check_output(["iwlist", "wlan0", "scan"])

        if not currently_connected:
            subprocess.check_call(["ip", "link", "set", "wlan0", "down"])

        return scan_output

    @classmethod
    def create_network(cls, name, *args, **kwargs):
        try:
            network_factory = cls.__data_network_factories[name]
        except KeyError as e:
            raise UnknownNetworkTypeError(name)

        return network_factory(*args, **kwargs)

    # TODO: migrate this to StoredNetwork
    #       The derived network registration should probably inform
    #       StoredNetwork about the relevant info files
    @classmethod
    def get_by_id(cls, network_id):
        network_path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(network_path):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        info_files = os.listdir(network_path)
        info_files.sort()
        network_factory = cls.__id_network_factories[tuple(info_files)]
        return network_factory(network_id)

    @classmethod
    def disconnect(cls):
        subprocess.call(["dhcpcd", "-x", "wlan0"])
        subprocess.call(["pkill", "wpa_supplicant"])
        subprocess.check_call(["ip", "link", "set", "wlan0", "down"])
        #TODO: fix this
        try:
            os.remove(os.path.join(cls.NETWORKS_DIR,
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

    @classmethod
    @abc.abstractmethod
    def _create_from_id(cls, network_id):
        pass

    @abc.abstractmethod
    def _specific_connect(self):
        pass

    @abc.abstractmethod
    def write_to_path(self, path):
        pass
