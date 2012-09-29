-module(cruxis_client).
-export([cruxis_client/0]).

connect_to_daemon() ->
    net_kernel:connect(cruxis_daemon@localhost).

call_daemon(Call) ->
    gen_server:call({cruxis_daemon, cruxis_daemon@localhost}, Call).

cruxis_client() ->
    [Command | Args] = init:get_plain_arguments(),
    case Command of
        "-h" -> help();
        "--help" -> help();
        "help" -> help();
        "auto_connect" -> auto_connect(Args);
        "connect" -> connect(Args);
        _ -> help()
    end.

help() ->
    io:format(standard_error,
"        usage: cruxis auto_connect
                cruxis connect -p (wep|wpa|unsecured) SSID
                cruxis connect -f WPA_CONF_FILE
                cruxis connect -n NETWORK_NUMBER
                cruxis help~n", []).

auto_connect([]) ->
    connect_to_daemon(),
    call_daemon(auto_connect);
auto_connect(_) ->
    help().

connect(["-p", "wep", Ssid]) ->
    connect_to_daemon(),
    io:format("Please enter the network key:~n"),
    call_daemon({connect, {wep, Ssid, io:get_password()}});
connect(["-p", "wpa", Ssid]) ->
    connect_to_daemon(),
    io:format("Please enter the network key:~n"),
    call_daemon({connect, {wpa, Ssid, io:get_password()}});
connect(["-p", "unsecured", Ssid]) ->
    connect_to_daemon(),
    call_daemon({connect, {unsecured, Ssid}});
connect(["-f", Wpa_conf_file]) ->
    connect_to_daemon(),
    call_daemon({connect, {wpa_from_file, Wpa_conf_file}});
connect(["-n", Network_number]) ->
    connect_to_daemon(),
    call_daemon({connect, {by_number, Network_number}});
connect(_) ->
    help().
