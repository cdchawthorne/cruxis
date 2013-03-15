import abc

class CruxisException(Exception, metaclass=abc.ABCMeta):
    __exceptions_dict = {}

    @classmethod
    def register_exception(cls, key, exception):
        cls.__exceptions_dict[key] = exception

    @abc.abstractproperty
    def key(self):
        pass

    @abc.abstractproperty
    def fields(self):
        pass

    @abc.abstractproperty
    def message_template(self):
        pass

    @classmethod
    def create_from_key(cls, key, fields):
        return cls.__exceptions_dict[key](*fields)

    @property
    def formatted_message(self):
        return self.message_template.format(self.fields)

    def __str__(self):
        return self.formatted_message


def cruxis_exception(name, key, message_template):
    assert key != "success", '''Key "success" is reserved'''

    class _DerivedExceptionMetaclass(abc.ABCMeta):
        def __new__(cls, ignored_name, bases, namespace):
            return super(_DerivedExceptionMetaclass, cls).__new__(
                    cls, name, bases, namespace)

    class DerivedException(CruxisException,
                           metaclass=_DerivedExceptionMetaclass):

        def __init__(self, *fields):
            self.__fields = fields

            try:
                self.formatted_message
            except IndexError:
                error_message = ("CruxisException [{}] initialized with"
                                 "insufficient number of fields")
                error_message = error_message.format(name)
                assert False, error_message

        @property
        def fields(self):
            return self.__fields

        @property
        def key(self):
            return key

        @property
        def message_template(self):
            return message_template

    CruxisException.register_exception(key, DerivedException)

    return DerivedException


AutoConnectError = cruxis_exception('AutoConnectError',
                                    'auto_connect_error',
                                    'AutoConnect failed')
BadKeyError = cruxis_exception('BadKeyError', 'bad_key_error', 
                               'Invalid WEP key for ssid {}')
BadWpaConfFileError = cruxis_exception(
        'BadWpaConfFileError',
        'bad_wpa_conf_file_error',
        'Bad WPA conf file: {}')
ConnectionError = cruxis_exception('ConnectionError',
                                   'connection_error', 
                                   'Error connecting to ssid {}')
CorruptNetworksFileError = cruxis_exception(
        'CorruptNetworksFileError',
        'corrupt_networks_file_error',
        'The networks file {} has been corrupted: id {} does not exist')
NetworkNotFoundError = cruxis_exception('NetworkNotFoundError',
                                        'network_not_found_error',
                                        'Network not found: {}')
NetworkStorageError = cruxis_exception(
        'NetworkStorageError', 'network_storage_error',
        'Error storing network; probably bad network parameters')
UnknownNetworkTypeError = cruxis_exception('UnknownNetworkTypeError',
                                           'unknown_network_type_error',
                                           'Unknown network type: {}')
UsageError = cruxis_exception('UsageError', 'usage_error',
                              'Error processing command line arguments')
