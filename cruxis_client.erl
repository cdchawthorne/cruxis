-module(cruxis_client).
-export([cruxis_client/0]).

connect_to_daemon() ->
    net_kernel:connect('cruxis_daemon@127.0.0.1').

call_daemon(Call) ->
    gen_server:call({cruxis_daemon, 'cruxis_daemon@127.0.0.1'}, Call).

cruxis_client() ->
    case init:get_plain_arguments() of
        ["-h" | _] -> help();
        ["--help" | _] -> help();
        ["help" | _] -> help();
        ["auto_connect"] -> auto_connect();
        ["connect" | Args ] -> connect(Args);
        ["add" | Args ] -> add_network(Args);
        ["remove", Network_id] -> remove_network(Network_id);
        ["remember", Network_id] -> remember_network(Network_id);
        ["forget", Network_id] -> forget_network(Network_id);
        ["list"] -> list_networks();
        _ -> help()
    end.

help() ->
    io:format(standard_error,
"        usage: cruxis auto_connect
                cruxis connect -p (wep|wpa|unsecured) SSID
                cruxis connect -f WPA_CONF_FILE
                cruxis connect -n NETWORK_NUMBER
                cruxis add -p (wep|wpa|unsecured) SSID
                cruxis add -f WPA_CONF_FILE
                cruxis remove NETWORK_ID
                cruxis remember NETWORK_ID
                cruxis forget NETWORK_ID
                cruxis help~n", []).

auto_connect() ->
    connect_to_daemon(),
    call_daemon(auto_connect).

drop_newline(Str) ->
    lists:foldr(fun(Char, Lst) ->
                    if Char =:= 10 andalso Lst =:= [] -> [];
                       true                           -> [Char | Lst ]
                    end
                end, [], Str).

connect(["-p", "wep", Ssid]) ->
    connect_to_daemon(),
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    call_daemon({connect, {wep, Ssid, Key}});
connect(["-p", "wpa", Ssid]) ->
    connect_to_daemon(),
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    call_daemon({connect, {wpa, Ssid, Key}});
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

add_network(["-p", "wep", Ssid]) ->
    connect_to_daemon(),
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    call_daemon({add_network, {wep, Ssid, Key}});
add_network(["-p", "wpa", Ssid]) ->
    connect_to_daemon(),
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    call_daemon({add_network, {wpa, Ssid, Key}});
add_network(["-p", "unsecured", Ssid]) ->
    connect_to_daemon(),
    call_daemon({add_network, {unsecured, Ssid}});
add_network(["-f", Wpa_conf_file]) ->
    connect_to_daemon(),
    call_daemon({add_network, {wpa_from_file, Wpa_conf_file}});
add_network(_) ->
    help().

remove_network(Network_id) ->
    connect_to_daemon(),
    call_daemon({remove_network, erlang:list_to_integer(Network_id)}).

remember_network(Network_id) ->
    connect_to_daemon(),
    call_daemon({remember_network, erlang:list_to_integer(Network_id)}).

forget_network(Network_id) ->
    connect_to_daemon(),
    call_daemon({forget_network, erlang:list_to_integer(Network_id)}).

list_networks() ->
    connect_to_daemon(),
    Res = call_daemon(list_networks),
    lists:map(fun({Network_id, Ssid}) -> io:format("~B: ~s~n", [Network_id, Ssid]) end, Res).
