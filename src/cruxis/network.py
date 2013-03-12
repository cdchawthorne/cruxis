import abc

import cruxis.connector
import cruxis.stored_network

class _NetworkMetaclass(abc.ABCMeta):
    def __init__(cls, name, bases, namespace):
        super(_NetworkMetaclass, cls).__init__(name, bases, namespace)
        if hasattr(cls, 'NAME') and hasattr(cls, 'INFO_FILES'):
            cruxis.connector.Connector.register_network_type(cls)
            cruxis.stored_network.StoredNetwork.register_network_type(cls)


class Network(cruxis.connector.Connector,
              cruxis.stored_network.StoredNetwork,
              metaclass=_NetworkMetaclass):
    pass
