-module(cruxis_daemon).
-export([start_daemon/0]).
-import(shell_calls).
-define(DAEMON_HANDLE, {cruxis_daemon, 'cruxis_daemon@127.0.0.1'}).

start_daemon() ->
    register(cruxis_daemon, self()),
    State = get_initial_state(),
    daemon(State).

get_initial_state() ->
    state_not_implemented.

daemon(State) ->
    io:format("foo"),
    receive
        {From, auto_connect} ->
            spawn_monitor(shell_calls, auto_connect, []),
            From ! {?DAEMON_HANDLE, success};
        {From, {connect, {by_number, N}}} ->
            spawn_monitor(shell_calls, connect_known_network, [N]),
            From ! {?DAEMON_HANDLE, success};
        {From, {connect, {wpa_from_file, File}}} ->
            spawn_monitor(shell_calls, connect_wpa, [File]),
            From ! {?DAEMON_HANDLE, success};
        {From, {connect, {wpa, Ssid, Key}}} ->
            spawn_monitor(shell_calls, connect_wpa, [Ssid, Key]),
            From ! {?DAEMON_HANDLE, success};
        {From, {connect, {wep, Ssid, Key}}} ->
            spawn_monitor(shell_calls, connect_wep, [Ssid, Key]),
            From ! {?DAEMON_HANDLE, success};
        {From, {connect, {unsecured, Ssid}}} ->
            spawn_monitor(shell_calls, connect_unsecured, [Ssid]),
            From ! {?DAEMON_HANDLE, success};
        {From, {add_network, {wpa_from_file, File}}} ->
            spawn_monitor(shell_calls, add_wpa_network, [File]),
            From ! {?DAEMON_HANDLE, success};
        {From, {add_network, {wpa, Ssid, Key}}} ->
            spawn_monitor(shell_calls, add_wpa_network, [Ssid, Key]),
            From ! {?DAEMON_HANDLE, success};
        {From, {add_network, {wep, Ssid, Key}}} ->
            spawn_monitor(shell_calls, add_wep_network, [Ssid, Key]),
            From ! {?DAEMON_HANDLE, success};
        {From, {add_network, {unsecured, Ssid}}} ->
            spawn_monitor(shell_calls, add_unsecured_network, [Ssid]),
            From ! {?DAEMON_HANDLE, success};
        {From, {remove_network, Network_id}} ->
            spawn_monitor(shell_calls, remove_network, [Network_id]),
            From ! {?DAEMON_HANDLE, success};
        {From, {remember_network, Network_id}} ->
            spawn_monitor(shell_calls, remember_network, [Network_id]),
            From ! {?DAEMON_HANDLE, success};
        {From, {forget_network, Network_id}} ->
            spawn_monitor(shell_calls, forget_network, [Network_id]),
            From ! {?DAEMON_HANDLE, success};
        {From, list_networks} ->
            From ! {?DAEMON_HANDLE, success, shell_calls:list_networks()};
        {From, _} ->
            From ! {?DAEMON_HANDLE, bad_call};
        _ -> nop
    end,
    daemon(State).
