-module(cruxis_daemon).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, start_server/0]).
-behaviour(gen_server).
-import(shell_calls).

start_server() ->
    gen_server:start({local, cruxis_daemon}, ?MODULE, [], []).

init(_) ->
    {ok, idle}.

handle_call(auto_connect, _, _) ->
    spawn_monitor(shell_calls, auto_connect, []),
    {reply, success, idle};
handle_call({connect, {by_number, N}}, _, _) ->
    spawn_monitor(shell_calls, connect_known_network, [N]),
    {reply, success, idle};
handle_call({connect, {wpa_from_file, File}}, _, _) ->
    spawn_monitor(shell_calls, connect_wpa, [File]),
    {reply, success, idle};
handle_call({connect, {wpa, Ssid, Key}}, _, _) ->
    spawn_monitor(shell_calls, connect_wpa, [Ssid, Key]),
    {reply, success, idle};
handle_call({connect, {wep, Ssid, Key}}, _, _) ->
    spawn_monitor(shell_calls, connect_wep, [Ssid, Key]),
    {reply, success, idle};
handle_call({connect, {unsecured, Ssid}}, _, _) ->
    spawn_monitor(shell_calls, connect_unsecured, [Ssid]),
    {reply, success, idle};
handle_call({add_network, {wpa_from_file, File}}, _, _) ->
    spawn_monitor(shell_calls, add_wpa_network, [File]),
    {reply, success, idle};
handle_call({add_network, {wpa, Ssid, Key}}, _, _) ->
    spawn_monitor(shell_calls, add_wpa_network, [Ssid, Key]),
    {reply, success, idle};
handle_call({add_network, {wep, Ssid, Key}}, _, _) ->
    spawn_monitor(shell_calls, add_wep_network, [Ssid, Key]),
    {reply, success, idle};
handle_call({add_network, {unsecured, Ssid}}, _, _) ->
    spawn_monitor(shell_calls, add_unsecured_network, [Ssid]),
    {reply, success, idle};
handle_call({remove_network, Network_id}, _, _) ->
    spawn_monitor(shell_calls, remove_network, [Network_id]),
    {reply, success, idle};
handle_call({remember_network, Network_id}, _, _) ->
    spawn_monitor(shell_calls, remember_network, [Network_id]),
    {reply, success, idle};
handle_call({forget_network, Network_id}, _, _) ->
    spawn_monitor(shell_calls, forget_network, [Network_id]),
    {reply, success, idle};
handle_call(list_networks, _, _) ->
    {reply, shell_calls:list_networks(), idle};
handle_call(_,_,_) ->
    {reply, bad_call, idle}.

handle_cast(_, _) ->
    {noreply, idle}.

handle_info(_, _) ->
    {noreply, idle}.

terminate(_, _) ->
    {}.

code_change(_, State, _) ->
    {ok, State}.
