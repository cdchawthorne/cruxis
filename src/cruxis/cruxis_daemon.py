import abc
import dbus
import dbus.service
import dbus.mainloop.glib
import functools
import gobject
import inspect
import os

import cruxis.connector
import cruxis.exceptions
import cruxis.networks_file

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
    NETWORKS_FILE = os.path.join(cruxis.connector.Connector.CRUXIS_DIR,
                                 'networks')
    PID_FILE = '/run/cruxis-daemon.pid'

    def __init__(self):
        super(CruxisDaemon, self).__init__(dbus.SystemBus(),
                                           CruxisDaemon.OBJECT_PATH)

        ret = dbus.SystemBus().request_name(CruxisDaemon.BUS_NAME)
        assert ret == dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER

        with open(self.PID_FILE, 'w') as f:
            f.write(str(os.getpid()) + "\n")

    ######################
    # Connection Methods #
    ######################

    @cruxis_method(in_signature='', out_signature='')
    def auto_connect(self):
        networks_file = cruxis.networks_file.NetworksFile(self.NETWORKS_FILE)
        network_ids = networks_file.remembered_ids

        try:
            remembered_networks = [
                    cruxis.stored_network.StoredNetwork.get_by_id(network_id)
                    for network_id in network_ids
                    ]
        except cruxis.exceptions.NetworkNotFoundError as e:
            raise cruxis.exceptions.CorruptNetworksFileError(self.NETWORKS_DIR,
                                                             e.fields[0])

        ssids = cruxis.connector.Connector.scan_ssids()
        for network in remembered_networks:
            if network.ssid in ssids:
                try:
                    network.connect()
                except cruxis.exceptions.ConnectionError:
                    pass
                else:
                    return

        raise cruxis.exceptions.AutoConnectError()

    @cruxis_method(in_signature='sas', out_signature='')
    def connect_network(self, network_type, network_args):

        network = cruxis.connector.Connector.create_network(network_type,
                                                            *network_args)

        network.connect()

    @cruxis_method(in_signature='i', out_signature='')
    def connect_by_id(self, network_id):
        network = cruxis.stored_network.StoredNetwork.get_by_id(network_id)
        network.connect()

    @cruxis_method(in_signature='', out_signature='')
    def disconnect(self):
        cruxis.connector.Connector.disconnect()

    @cruxis_method(in_signature='', out_signature='(bs)')
    def connected_network(self):
        connected_ssid = cruxis.connector.Connector.connected_ssid()
        if connected_ssid is not None:
            return (True, connected_ssid)
        else:
            return (False, '')

    @cruxis_method(in_signature='', out_signature='s')
    def scan(self):
        return cruxis.connector.Connector.scan()

    def run_maintenance(self):
        return True


    ########################################
    # Methods for managing stored networks #
    ########################################

    @cruxis_method(in_signature='sas', out_signature='')
    def add_network(self, network_type, network_args):
        network = cruxis.connector.Connector.create_network(network_type,
                                                            *network_args)

        network.store()

    @cruxis_method(in_signature='i', out_signature='')
    def remove_network(self, network_id):
        if not cruxis.stored_network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(str(network_id))

        networks_file = cruxis.networks_file.NetworksFile(self.NETWORKS_FILE)
        networks_file.forget_network(network_id)
        cruxis.stored_network.StoredNetwork.remove_network(network_id)

    @cruxis_method(in_signature='', out_signature='a(is)')
    def list_networks(self):
        return [(network_id,
                 cruxis.stored_network.StoredNetwork.get_by_id(network_id).ssid)
                for network_id
                in cruxis.stored_network.StoredNetwork.used_ids()]

    @cruxis_method(in_signature='', out_signature='a(is)')
    def list_remembered_networks(self):
        networks_file = cruxis.networks_file.NetworksFile(self.NETWORKS_FILE)
        remembered_ids = networks_file.remembered_ids

        def network_pair(network_id):
            network = cruxis.stored_network.StoredNetwork.get_by_id(network_id)
            return (network_id, network.ssid)

        try:
            return [network_pair(network_id) for network_id in remembered_ids]
        except cruxis.exceptions.NetworkNotFoundError as e:
            raise cruxis.exceptions.CorruptNetworksFileError(self.NETWORKS_FILE,
                                                             e.fields[0])

    @cruxis_method(in_signature='i', out_signature='')
    def remember_network(self, network_id):
        if not cruxis.stored_network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(str(network_id))

        networks_file = cruxis.networks_file.NetworksFile(self.NETWORKS_FILE)
        networks_file.remember_network(network_id)

    @cruxis_method(in_signature='i', out_signature='')
    def forget_network(self, network_id):
        if not cruxis.stored_network.StoredNetwork.is_id_used(network_id):
            raise cruxis.exceptions.NetworkNotFoundError(str(network_id))

        networks_file = cruxis.networks_file.NetworksFile(self.NETWORKS_FILE)
        networks_file.forget_network(network_id)


if __name__ == '__main__':
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
    c = CruxisDaemon()
    #gobject.timeout_add(1000, c.run_maintenance)
    loop = gobject.MainLoop()
    loop.run()
