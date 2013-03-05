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


class CruxisClient:
    NETWORK_TYPE_WPA = 'wpa'
    NETWORK_TYPE_WEP = 'wep'
    NETWORK_TYPE_UNSECURED = 'unsecured'

    CLI_ARG_WPA = 'wpa'
    CLI_ARG_WEP = 'wep'
    CLI_ARG_UNSECURED = 'unsecured'

    __network_types = {CLI_ARG_WPA:NETWORK_TYPE_WPA,
                       CLI_ARG_WEP:NETWORK_TYPE_WEP,
                       CLI_ARG_UNSECURED:NETWORK_TYPE_UNSECURED}

    USAGE_MESSAGE = '''usage:
            cruxis auto_connect
            cruxis connect (wpa|wep|unsecured) SSID
            cruxis connect WPA_CONF_FILE
            cruxis connect_by_id NETWORK_ID_NUM
            cruxis add (wpa|wep|unsecured) SSID
            cruxis add WPA_CONF_FILE
            cruxis scan
            cruxis remove NETWORK_ID_NUM
            cruxis remember NETWORK_ID_NUM
            cruxis forget NETWORK_ID_NUM
            cruxis list
            cruxis list_remembered
            cruxis status
            cruxis help'''
           
    def __init__(self):
        self.__daemon = _DaemonProxy()

    def __get_command(self, command):
        commands_dict = {
                'auto_connect': self.auto_connect,
                'ac': self.auto_connect,
                'connect': self.connect,
                'connect_by_id': self.connect_by_id,
                'ci': self.connect_by_id,
                'add': self.add,
                'scan': self.scan,
                'remove': self.remove,
                'remember': self.remember,
                'forget': self.forget,
                'list': self.list_networks,
                'list_remembered': self.list_remembered_networks,
                'disconnect': self.disconnect,
                'status': self.status,
                'help': self.print_usage,
                '-h': self.print_usage,
                '--help': self.print_usage,
                }

        return commands_dict[command]

    def run_command(self, args):
        try:
            command = self.__get_command(args[0])
        except KeyError:
            # Bad command
            self.print_usage()
            return
        

        try:
            command(*args[1:])
        except TypeError:
            # Wrong number of arguments
            self.print_usage()
        except cruxis.exceptions.UsageError:
            self.print_usage()

    def auto_connect(self):
        self.__daemon.auto_connect(timeout=152)

    def list_networks(self):
        for network_id, ssid in self.__daemon.list_networks():
            print('{}: {}'.format(network_id, ssid))

    def list_remembered_networks(self):
        for network_id, ssid in self.__daemon.list_remembered_networks():
            print('{}: {}'.format(network_id, ssid))

    def remember(self, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        self.__daemon.remember_network(network_id)

    def forget(self, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        self.__daemon.forget_network(network_id)

    def connect_by_id(self, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        self.__daemon.connect_by_id(network_id, timeout=32)

    def connect(self, *args, **kwargs):
        if len(args) + len(kwargs) == 1:
            self.connect_from_file(*args, **kwargs)
        else:
            self.connect_from_data(*args, **kwargs)

    def connect_from_file(self, filename):
        self.__daemon.connect_network(self.NETWORK_TYPE_WPA, [filename],
                                      timeout=32)

    def connect_from_data(self, protocol, ssid):
        connect_args = [ssid]

        if protocol in [self.CLI_ARG_WPA, self.CLI_ARG_WEP]:
            key = getpass.getpass('Enter network key: ')
            connect_args.append(key)

        self.__daemon.connect_network(self.__network_types[protocol],
                                      connect_args, timeout=32)

    def add(self, *args, **kwargs):
        if len(args) + len(kwargs) == 1:
            self.add_from_file(*args, **kwargs)
        else:
            self.add_from_data(*args, **kwargs)

    def add_from_file(self, filename):
        self.__daemon.add_network(self.NETWORK_TYPE_WPA, [filename])

    def add_from_data(self, protocol, ssid):
        add_args = [ssid]

        if protocol in [self.CLI_ARG_WPA, self.CLI_ARG_WEP]:
            key = getpass.getpass('Enter network key: ')
            add_args.append(key)

        self.__daemon.add_network(self.__network_types[protocol],
                                  add_args)

    def scan(self):
        print(self.__daemon.scan())

    def remove(self, network_id_str):
        try:
            network_id = int(network_id_str)
        except ValueError:
            raise cruxis.exceptions.UsageError()

        self.__daemon.remove_network(network_id)

    def disconnect(self):
        self.__daemon.disconnect()

    def status(self):
        connected = self.__daemon.connected()
        if connected:
            print('Connected')
        else:
            print('Disconnected')

    def print_usage(self):
        print(self.USAGE_MESSAGE, file=sys.stderr)


if __name__ == "__main__":
    CruxisClient().run_command(sys.argv[1:])
