import abc
import dbus
import dbus.service
import dbus.mainloop.glib
import functools
import gobject
import inspect
import os

import cruxis.exceptions
import cruxis.network
import cruxis.network_file

class CruxisMethod:
    def __init__(self, in_signature='', out_signature='', default_value=None):
        self.__in_signature = in_signature
        self.__returns = out_signature != ''
        self.__out_signature = '(sas{sig})'.format(sig=out_signature)
        self.__default_value = CruxisMethod.default_value(out_signature)

    @staticmethod
    def default_value(sig):
        def parse(sig):
            if not sig:
                return None, ''
            elif sig[0] == '(':
                sig = sig[1:]
                vals = []
                while sig[0] != ')':
                    val, sig = parse(sig)
                    vals.append(val)
                
                return tuple(vals), sig[1:]

            elif sig[0] == '{':
                sig = sig[1:]
                _, sig = parse(sig)
                _, sig = parse(sig)
                assert sig[0] == '}'

                return {}, sig[1:]

            elif sig[0] == 'a':
                _, sig = parse(sig[1:])
                return [], sig
            elif sig[0] in 'bynixqutd':
                return 0, sig[1:]
            elif sig[0] == 's':
                return '', sig[1:]

            else:
                assert False, "Unimplemented signature"

        val, rest = parse(sig)
        assert "" == rest
        return val

    def __return_tup(self, key, fields, value):
        if self.__returns:
            return key, fields, value
        else:
            return key, fields

    def __wrap(self, method):

        def wrapped_method(*args, **kwargs):
            try:
                return self.__return_tup("success", [], method(*args, **kwargs))
            except cruxis.exceptions.CruxisException as e:
                return self.__return_tup(e.key, e.fields, self.__default_value)

        formatted_args = inspect.formatargspec(*inspect.getargspec(method))
        fakefn_def = 'lambda {args}: wrapped_method({args})'.format(
                args=formatted_args.rstrip(')').lstrip('('))
        fakefn = eval(fakefn_def, {'wrapped_method':wrapped_method})
        return functools.wraps(method)(fakefn)

    def __call__(self, func):

        dbus_decorator = dbus.service.method(
                dbus_interface='org.zelos.cruxis.daemon_interface',
                in_signature=self.__in_signature,
                out_signature=self.__out_signature)

        return dbus_decorator(self.__wrap(func))

cruxis_method = CruxisMethod


class CruxisDaemon(dbus.service.Object):
    BUS_NAME = 'org.zelos.cruxis.daemon'
    OBJECT_PATH = '/org/zelos/cruxis/daemon'
    INTERFACE = 'org.zelos.cruxis.daemon_interface'
    NETWORK_FILE = os.path.join(cruxis.network.Network.CRUXIS_DIR,
                                'networks')
    PID_FILE = '/run/cruxis-daemon.pid'

    def __init__(self):
        super(CruxisDaemon, self).__init__(dbus.SystemBus(),
                                           CruxisDaemon.OBJECT_PATH)

        ret = dbus.SystemBus().request_name(CruxisDaemon.BUS_NAME)
        assert ret == dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER

        with open(self.PID_FILE, 'w') as f:
            f.write(str(os.getpid()) + "\n")

        self.__connected = False

    # Makes sure that self.__connected is set correctly when attempting a
    # connection
    def __connect_to(self, network):
        self.__connected = False
        network.connect()
        self.__connected = True

    @cruxis_method(in_signature='', out_signature='')
    def auto_connect(self):
        network_file = cruxis.network_file.NetworkFile(self.NETWORK_FILE)
        network_ids = network_file.remembered_ids

        try:
            remembered_networks = [cruxis.network.Network.get_by_id(network_id)
                                   for network_id in network_ids]
        except cruxis.exceptions.NetworkNotFoundError as e:
            raise cruxis.exceptions.CorruptNetworksFileError(self.NETWORKS_DIR,
                                                             e.fields[0])

        ssids = cruxis.network.Network.scan_ssids()
        for network in remembered_networks:
            if network.ssid in ssids:
                try:
                    self.__connect_to(network)
                except cruxis.exceptions.ConnectionError:
                    pass
                else:
                    return

        raise cruxis.exceptions.AutoConnectError()

    @cruxis_method(in_signature='i', out_signature='')
    def connect_by_id(self, network_id):
        network = cruxis.network.Network.get_by_id(network_id)
        self.__connect_to(network)

    @cruxis_method(in_signature='sas', out_signature='')
    def add_network(self, network_type, network_args):
        network = cruxis.network.Network.create_network(network_type,
                                                        *network_args)

        next_id = cruxis.network.StoredNetwork.next_unused_id()
        cruxis.network.StoredNetwork.store_at_id(network, next_id)

    @cruxis_method(in_signature='i', out_signature='')
    def remove_network(self, network_id):
        if not cruxis.network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        network_file = cruxis.network_file.NetworkFile(self.NETWORK_FILE)
        network_file.forget_network(network_id)
        cruxis.network.StoredNetwork.remove_network(network_id)

    @cruxis_method(in_signature='sas', out_signature='')
    def connect_network(self, network_type, network_args):

        network = cruxis.network.Network.create_network(network_type,
                                                        *network_args)

        self.__connect_to(network)

    @cruxis_method(in_signature='', out_signature='a(is)')
    def list_networks(self):
        return [(network_id, cruxis.network.Network.get_by_id(network_id).ssid)
                for network_id in cruxis.network.StoredNetwork.used_ids()]

    @cruxis_method(in_signature='', out_signature='a(is)')
    def list_remembered_networks(self):
        network_file = cruxis.network_file.NetworkFile(self.NETWORK_FILE)
        remembered_ids = network_file.remembered_ids
        try:
            return [(network_id,
                     cruxis.network.Network.get_by_id(network_id).ssid)
                    for network_id in remembered_ids]
        except cruxis.exceptions.NetworkNotFoundError as e:
            raise cruxis.exceptions.CorruptNetworksFileError(self.NETWORK_FILE,
                                                             e.fields[0])

    @cruxis_method(in_signature='i', out_signature='')
    def remember_network(self, network_id):
        if not cruxis.network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        network_file = cruxis.network_file.NetworkFile(self.NETWORK_FILE)
        network_file.remember_network(network_id)

    @cruxis_method(in_signature='i', out_signature='')
    def forget_network(self, network_id):
        if not cruxis.network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(network_id)

        network_file = cruxis.network_file.NetworkFile(self.NETWORK_FILE)
        network_file.forget_network(network_id)

    @cruxis_method(in_signature='', out_signature='')
    def disconnect(self):
        self.__connected = False
        cruxis.network.Network.disconnect()

    @cruxis_method(in_signature='', out_signature='b')
    def connected(self):
        return cruxis.network.Network.check_connected()

    @cruxis_method(in_signature='', out_signature='s')
    def scan(self):
        return cruxis.network.Network.scan(self.__connected)

    def run_maintenance(self):
        return True


if __name__ == '__main__':
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
    c = CruxisDaemon()
    #gobject.timeout_add(1000, c.run_maintenance)
    loop = gobject.MainLoop()
    loop.run()
