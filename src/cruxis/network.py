import abc
import os
import re
import shutil
import subprocess

import cruxis.exceptions

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

def network_type(cls):
    Network._register_network(cls)

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
    def get_factory_by_name(cls, name):
        return cls.__data_network_factories[name]

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

    def write_to_fs(self, network_id):
        path = os.path.join(self.NETWORKS_DIR, str(network_id))
        if os.path.exists(path):
            raise cruxis.exceptions.NetworkIdInUseError(network_id)

        os.mkdir(path)

        self._write_to_path(path)

    @classmethod
    def remove_from_fs(cls, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(path):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        shutil.rmtree(path)

    @abc.abstractmethod
    def _write_to_path(self, path):
        pass
