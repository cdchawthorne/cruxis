import abc

#TODO: have DerivedException assert that the right number of arguments are
#      given for MESSAGE_TEMPLATE

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


# TODO: use type to make the name correctly
def cruxis_exception(key, message_template):
    assert key != "success", '''Key "success" is reserved'''

    class DerivedException(CruxisException):
        def __init__(self, *fields):
            self.__fields = fields

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


ConnectionError = cruxis_exception('connection_error', 
                                   'Error connecting to ssid {}')
AutoConnectError = cruxis_exception('auto_connect_error',
                                    'AutoConnect failed')
NetworkNotFoundError = cruxis_exception('network_not_found_error',
                                        'Network not found: {}')
UnknownNetworkTypeError = cruxis_exception('unknown_network_type_error',
                                           'Unknown network type: {}')
UsageError = cruxis_exception('usage_error',
                              'Error processing command line arguments')
CorruptNetworksFileError = cruxis_exception(
        'corrupt_networks_file_error',
        'The networks file {} has been corrupted: id {} does not exist')
BadWpaConfFileError = cruxis_exception(
        'bad_wpa_conf_file_error',
        'Bad WPA conf file: {}')
BadKeyError = cruxis_exception('bad_key_error', 'Invalid WEP key for ssid {}')
