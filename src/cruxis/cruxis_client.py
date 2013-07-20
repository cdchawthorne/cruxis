import dbus
import getpass
import sys
import types

import cruxis.cruxis_daemon
import cruxis.exceptions


class _DaemonProxy:
    def __init__(self):
        bus = dbus.SystemBus()
        self.__daemon = bus.get_object(
                cruxis.cruxis_daemon.CruxisDaemon.BUS_NAME,
                cruxis.cruxis_daemon.CruxisDaemon.OBJECT_PATH)

    def __getattr__(self, name):
        attr = getattr(self.__daemon, name)

        if callable(attr):
            return self.__wrap_method(attr)

        return attr

    def __wrap_method(self, method):
        def wrapped(*args, **kwargs):
            assert 'dbus_interface' not in kwargs
            kwargs['dbus_interface'] = (
                    cruxis.cruxis_daemon.CruxisDaemon.INTERFACE)

            packed = method(*args, **kwargs)

            try:
                error_key, fields, ret = packed
            except ValueError:
                error_key, fields = packed
                ret = None

            if error_key == "success":
                return ret
            else:
                raise cruxis.exceptions.CruxisException.create_from_key(
                        error_key, fields)

        return wrapped


class _ClientMethod:
    __methods_dict = {}

    @classmethod
    def _register_method(cls, names, method):
        for name in names:
            cls.__methods_dict[name] = method

    @classmethod
    def get_method_by_name(cls, name):
        return cls.__methods_dict[name]

    def __init__(self, fn, message, options=[]):
        self.__fn = fn
        self.__message = message
        self.__options = options

    def __call__(self, *args, **kwargs):
        return self.__fn(CruxisClient, *args, **kwargs)

    @property
    def help(self):
        option_lines = ['{}    {}'.format(option, option_help)
                        for option, option_help in self.__options]
        return '\n'.join([self.__message] + option_lines)


def client_method(names, message, options=[]):
    def generator(fn):
        method = _ClientMethod(fn, message, options)
        _ClientMethod._register_method(names, method)
        return method

    return generator


# TODO: give more useful error messages
#       Go through the client code with an eye towards the user-supplied data
class CruxisClient:
    NETWORK_TYPE_WPA = 'wpa'
    NETWORK_TYPE_WEP = 'wep'
    NETWORK_TYPE_UNSECURED = 'unsecured'

    CLI_ARG_WPA = 'wpa'
    CLI_ARG_WEP = 'wep'
    CLI_ARG_UNSECURED = 'unsecured'

    __network_types = {CLI_ARG_WPA: NETWORK_TYPE_WPA,
                       CLI_ARG_WEP: NETWORK_TYPE_WEP,
                       CLI_ARG_UNSECURED: NETWORK_TYPE_UNSECURED}

    __daemon = _DaemonProxy()

    USAGE_MESSAGE = '''usage:
            cruxis auto_connect
            cruxis connect (wpa|wep|unsecured) SSID
            cruxis connect WPA_CONF_FILE
            cruxis connect_by_id NETWORK_ID_NUM
            cruxis add [-r] (wpa|wep|unsecured) SSID
            cruxis add WPA_CONF_FILE
            cruxis scan
            cruxis remove NETWORK_ID_NUM
            cruxis remember NETWORK_ID_NUM
            cruxis forget NETWORK_ID_NUM
            cruxis list
            cruxis list_remembered
            cruxis status
            cruxis help
            cruxis help COMMAND'''

    @classmethod
    def run_command(cls, args):
        if not args:
            cls.print_usage()
            sys.exit(1)

        try:
            command = _ClientMethod.get_method_by_name(args[0])
        except KeyError:
            # Bad command
            cls.print_usage()
            sys.exit(1)

        try:
            command(*args[1:])
        except cruxis.exceptions.UsageError:
            cls.print_usage()
            sys.exit(1)

    @client_method(['ac', 'auto_connect'],
                   'Attempt to connect to remembered networks')
    def auto_connect(cls):
        cls.__daemon.auto_connect(timeout=152)

    @client_method(['list'], 'List known networks')
    def list_networks(cls):
        for network_id, ssid in cls.__daemon.list_networks():
            print('{}: {}'.format(network_id, ssid))

    @client_method(['list_remembered'], 'List remembered networks')
    def list_remembered_networks(cls):
        for network_id, ssid in cls.__daemon.list_remembered_networks():
            print('{}: {}'.format(network_id, ssid))

    @client_method(['remember'], 'Remember network')
    def remember(cls, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        cls.__daemon.remember_network(network_id)

    @client_method(['forget'], 'Forget network')
    def forget(cls, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        cls.__daemon.forget_network(network_id)

    @client_method(['connect_by_id'], 'Connect to known network with given ID')
    def connect_by_id(cls, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        cls.__daemon.connect_by_id(network_id, timeout=32)

    @client_method(['connect'], 'Connect to network with given parameters')
    def connect(cls, *args, **kwargs):
        if len(args) + len(kwargs) == 1:
            cls.connect_from_file(*args, **kwargs)
        elif len(args) + len(kwargs) == 2:
            cls.connect_from_data(*args, **kwargs)
        else:
            raise cruxis.exceptions.UsageError()

    @classmethod
    def connect_from_file(cls, filename):
        cls.__daemon.connect_network(cls.NETWORK_TYPE_WPA, [filename],
                                     timeout=32)

    @classmethod
    def connect_from_data(cls, protocol, ssid):
        connect_args = [ssid]

        if protocol in [cls.CLI_ARG_WPA, cls.CLI_ARG_WEP]:
            key = getpass.getpass('Enter network key: ')
            connect_args.append(key)

        cls.__daemon.connect_network(cls.__network_types[protocol],
                                     connect_args, timeout=32)

    @client_method(['add'],
                  'Add network with given parameters to '
                  'the list of known networks',
                  options=[('-r', 'Remember this network')])
    def add(cls, *args, **kwargs):
        if len(args) + len(kwargs) == 1:
            cls.add_from_file(*args, **kwargs)
        elif len(args) + len(kwargs) == 2:
            cls.add_from_data(*args, **kwargs)
        else:
            raise cruxis.exceptions.UsageError()

    @classmethod
    def add_from_file(cls, filename):
        cls.__daemon.add_network(cls.NETWORK_TYPE_WPA, [filename])

    @classmethod
    def add_from_data(cls, protocol, ssid):
        add_args = [ssid]

        if protocol in [cls.CLI_ARG_WPA, cls.CLI_ARG_WEP]:
            key = getpass.getpass('Enter network key: ')
            add_args.append(key)

        cls.__daemon.add_network(cls.__network_types[protocol],
                                 add_args)

    @client_method(['scan'], 'Scan interface for networks')
    def scan(cls):
        print(cls.__daemon.scan())

    @client_method(['remove'],
                  'Remove network with given ID from the list '
                  'of remembered networks')
    def remove(cls, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        cls.__daemon.remove_network(network_id)

    @client_method(['disconnect'],
                   'Disconnect from currently connected network')
    def disconnect(cls):
        cls.__daemon.disconnect()

    @client_method(['status'], 'Check if currently connected to a network')
    def status(cls):
        connected, ssid = cls.__daemon.connected_network()
        if connected:
            print('Connected to {}'.format(ssid))
        else:
            print('Disconnected')

    @client_method(['help'], 'Get help on a given command')
    def help(cls, *args, **kwargs):
        if len(args) + len(kwargs) == 1:
            try:
                method = _ClientMethod.get_method_by_name(*args, **kwargs)
            except KeyError:
                raise cruxis.exceptions.UsageError()

            print(method.help)
        elif len(args) + len(kwargs) == 0:
            cls.print_usage()
        else:
            raise cruxis.exceptions.UsageError()

    @classmethod
    def print_usage(cls):
        print(CruxisClient.USAGE_MESSAGE, file=sys.stderr)


if __name__ == "__main__":
    CruxisClient.run_command(sys.argv[1:])
