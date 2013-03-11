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
    def __next_unused_id(cls):
        used_ids = frozenset(int(id_str)
                             for id_str 
                             in os.listdir(cls.NETWORKS_DIR))

        i = 0
        while i in used_ids:
            i = i + 1

        return i

    @classmethod
    def _register_subclass(cls, network_type):
        info_files = network_type.INFO_FILES
        assert info_files not in cls.__network_types
        cls.__network_types[info_files] = network_type

    @classmethod
    def used_ids(cls):
        '''Return a sorted list of the currently used network IDs'''
        ids = [int(id_str) for id_str in os.listdir(cls.NETWORKS_DIR)]
        ids.sort()
        return ids

    @classmethod
    def is_id_used(cls, network_id):
        '''
        Check whether the id is in use. Quicker than checking membership
        of cls.used_ids() (I think).
        '''
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        return os.path.exists(path)

    @classmethod
    def get_by_id(cls, network_id):
        '''
        Get a network by its network ID.
        Return an instance of StoredNetwork.
        '''
        network_path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(network_path):
            raise cruxis.exceptions.NetworkNotFoundError(str(network_id))

        info_files = os.listdir(network_path)
        info_files.sort()
        network_type = cls.__network_types[tuple(info_files)]
        return network_type._get_by_path(network_path)

    @classmethod
    def remove_network(cls, network_id):
        '''Delete a network from the filesystem'''
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(path):
            raise cruxis.exceptions.NetworkNotFoundError(str(network_id))

        shutil.rmtree(path)

    def store(self):
        '''Store network information to a new network ID in the filesystem'''
        network_id = self.__next_unused_id()
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        assert not os.path.exists(path)
        os.mkdir(path)

        self._write_to_path(path)

    @abc.abstractmethod
    def _write_to_path(self, path):
        '''Store network information to the pre-existing directory path'''

    @classmethod
    @abc.abstractmethod
    def _get_by_path(cls, path):
        '''
        Retrieve network information from the pre-existing directory path.
        Returns an instance of cls.
        '''
