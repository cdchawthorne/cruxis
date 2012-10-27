-module(cruxis_client).
-export([cruxis_client/0]).
-define(DAEMON_HANDLE, {cruxis_daemon, 'cruxis_daemon@127.0.0.1'}).

connect_to_daemon() ->
    net_kernel:connect('cruxis_daemon@127.0.0.1').

send_daemon(Msg) ->
    ?DAEMON_HANDLE ! {self(), Msg}.

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
    send_daemon(auto_connect),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: auto_connect error: ~s~n", [Reason])
    after
        5000 ->
            io:format(standard_error,
                "cruxis: auto_connect error: timeout waiting for server", [])
    end.

drop_newline(Str) ->
    lists:foldr(fun(Char, Lst) ->
                    if Char =:= 10 andalso Lst =:= [] -> [];
                       true                           -> [Char | Lst ]
                    end
                end, [], Str).

connect(Args) ->
    connect_to_daemon(),
    connect_by_protocol(Args),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: connect error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: connect error: timeout waiting for server~n", [])
    end.

connect_by_protocol(["-p", "wep", Ssid]) ->
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    send_daemon({connect, {wep, Ssid, Key}});
connect_by_protocol(["-p", "wpa", Ssid]) ->
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    send_daemon({connect, {wpa, Ssid, Key}});
connect_by_protocol(["-p", "unsecured", Ssid]) ->
    send_daemon({connect, {unsecured, Ssid}});
connect_by_protocol(["-f", Wpa_conf_file]) ->
    send_daemon({connect, {wpa_from_file, Wpa_conf_file}});
connect_by_protocol(["-n", Network_number]) ->
    send_daemon({connect, {by_number, Network_number}});
connect_by_protocol(_) ->
    help().

add_network(Args) ->
    connect_to_daemon(),
    add_network_by_protocol(Args),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: add_network error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: add_network error: timeout waiting for server~n", [])
    end.

add_network_by_protocol(["-p", "wep", Ssid]) ->
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    send_daemon({add_network, {wep, Ssid, Key}});
add_network_by_protocol(["-p", "wpa", Ssid]) ->
    %io:format("Please enter the network key:~n"),
    %Key = io:get_password(),
    Key = drop_newline(io:get_line("")),
    send_daemon({add_network, {wpa, Ssid, Key}});
add_network_by_protocol(["-p", "unsecured", Ssid]) ->
    send_daemon({add_network, {unsecured, Ssid}});
add_network_by_protocol(["-f", Wpa_conf_file]) ->
    {ok, Cwd} = file:get_cwd(),
    send_daemon({add_network, {wpa_from_file, Cwd ++ "/" ++ Wpa_conf_file}});
add_network_by_protocol(_) ->
    help().

remove_network(Network_id) ->
    connect_to_daemon(),
    send_daemon({remove_network, erlang:list_to_integer(Network_id)}),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: remove_network error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: remove_network error: timeout waiting for server~n", [])
    end.

remember_network(Network_id) ->
    connect_to_daemon(),
    send_daemon({remember_network, erlang:list_to_integer(Network_id)}),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: remember_network error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: remember_network error: timeout waiting for server~n", [])
    end.

forget_network(Network_id) ->
    connect_to_daemon(),
    send_daemon({forget_network, erlang:list_to_integer(Network_id)}),
    receive
        {?DAEMON_HANDLE, success} -> success;
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: forget_network error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: forget_network error: timeout waiting for server~n", [])
    end.

list_networks() ->
    connect_to_daemon(),
    send_daemon(list_networks),
    receive
        {?DAEMON_HANDLE, success, Res} ->
            lists:map(
                fun({Network_id, Ssid}) ->
                    io:format("~B: ~s~n", [Network_id, Ssid]) end,
                Res);
        {?DAEMON_HANDLE, {error, Reason}} ->
            io:format(standard_error, "cruxis: list_networks error: ~s~n", [Reason])
    after
        5000 -> 
            io:format(standard_error,
                "cruxis: list_networks error: timeout waiting for server~n", [])
    end.
