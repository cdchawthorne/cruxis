import os

import cruxis.exceptions
class StoredNetwork:
    NETWORKS_DIR = '/etc/cruxis/networks.d'

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
    def remove_network(cls, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if not os.path.exists(path):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        shutil.rmtree(path)

    @classmethod
    def store_at_id(cls, network, network_id):
        path = os.path.join(cls.NETWORKS_DIR, str(network_id))
        if os.path.exists(path):
            raise cruxis.exceptions.NetworkIdInUseError(network_id)

        os.mkdir(path)

        network.write_to_path(path)
