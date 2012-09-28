-module(cruxis_daemon).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, start_server/0]).
-behaviour(gen_server).
-import(shell_calls).

start_server() ->
    gen_server:start({local, cruxis_daemon}, ?MODULE, [], []).

init(Args) ->
    {ok, idle}.

handle_call(auto_connect, From, State) ->
    spawn_monitor(shell_calls, auto_connect, []),
    {reply, success, idle};
handle_call({connect, {by_number, N}}, From, State) ->
    spawn_monitor(shell_calls, connect_known_network, [N]),
    {reply, success, idle};
handle_call({connect, {wpa_from_file, File}}, From, State) ->
    spawn_monitor(shell_calls, connect_wpa, [File]),
    {reply, success, idle};
handle_call({connect, {wpa, Ssid, Key}}, From, State) ->
    spawn_monitor(shell_calls, connect_wpa, [Ssid, Key]),
    {reply, success, idle};
handle_call({connect, {wep, Ssid, Key}}, From, State) ->
    spawn_monitor(shell_calls, connect_wep, [Ssid, Key]),
    {reply, success, idle};
handle_call({connect, {unsecured, Ssid}}, From, State) ->
    spawn_monitor(shell_calls, connect_unsecured, [Ssid]),
    {reply, success, idle}.

handle_cast(Request, State) ->
    {noreply, idle}.

handle_info(Info, State) ->
    {noreply, idle}.

terminate(Reason, State) ->
    {}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
