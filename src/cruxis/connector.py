import abc
import os
import re
import shutil
import subprocess

import cruxis.exceptions

class Connector(metaclass=abc.ABCMeta):
    CRUXIS_DIR = '/etc/cruxis'
    TEST_URL = 'xkcd.com'
    INTERFACE = 'wlan0'

    __network_types = {}

    # This can be a dictionary if we later support multiple interfaces
    __connected_network = None

    @classmethod
    def register_network_type(cls, network_type):
        assert hasattr(network_type, 'NAME')
        name = network_type.NAME
        assert name not in cls.__network_types
        cls.__network_types[name] = network_type

    @classmethod
    def scan(cls):
        if cls.__connected_network is None:
            subprocess.check_call(["ip", "link", "set", cls.INTERFACE, "up"])

        scan_output = subprocess.check_output(
                ["iwlist", cls.INTERFACE, "scan"])

        if cls.__connected_network is None:
            subprocess.check_call(
                    ["ip", "link", "set", cls.INTERFACE, "down"])

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
        if cls.__connected_network is not None:
            cls.__connected_network._specific_disconnect()
            cls.__connected_network = None

    @classmethod
    def check_connected(cls):
        #ret = subprocess.call(
                #["ping", "-c3", "-I", cls.INTERFACE, cls.TEST_URL])
        #return ret == 0
        return cls.__connected_network is not None

    def connect(self):
        Connector.disconnect()
        try:
            self._specific_connect()
        except cruxis.exceptions.ConnectionError:
            raise
        else:
            Connector.__connected_network = self

    @classmethod
    def scan_ssids(cls):
        if cls.__connected_network is None:
            subprocess.check_call(["ip", "link", "set", cls.INTERFACE, "up"])

        scan_output = subprocess.check_output(
                ["iwlist", cls.INTERFACE, "scan"])

        if cls.__connected_network is None:
            subprocess.check_call(["ip", "link", "set", cls.INTERFACE, "down"])

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

    @abc.abstractmethod
    def _specific_disconnect(self):
        pass
