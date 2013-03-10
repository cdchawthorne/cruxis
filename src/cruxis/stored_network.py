import abc
import os

import cruxis.exceptions

def stored_network(cls):
    StoredNetwork._register_subclass(cls)
    return cls

class StoredNetwork(metaclass=abc.ABCMeta):
    NETWORKS_DIR = '/etc/cruxis/networks.d'
    __network_types = {}

    @classmethod
    def _register_subclass(cls, network_type):
        info_files = network_type.INFO_FILES
        assert info_files not in cls.__network_types
        cls.__network_types[info_files] = network_type

    # TODO: for the love of God, Montresor, change this interface
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
    def get_by_id(cls, network_id):
        network_path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(network_path):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        info_files = os.listdir(network_path)
        info_files.sort()
        network_type = cls.__network_types[tuple(info_files)]
        return network_type._get_by_path(network_path)

    @classmethod
    def remove_network(cls, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(path):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        shutil.rmtree(path)

    def store_at_id(self, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if os.path.exists(path):
            raise cruxis.exceptions.NetworkIdInUseError(network_id)

        os.mkdir(path)

        self._write_to_path(path)

    @abc.abstractmethod
    def _write_to_path(self, path):
        pass

    @classmethod
    @abc.abstractmethod
    def _get_by_path(self, path):
        pass
