-module(shell_calls).
-export([connect_wep/2, connect_wpa/1, connect_wpa/2, connect_unsecured/1,
         connect_known_network/1, auto_connect/0, add_wpa_network/1,
         add_wpa_network/2, add_wep_network/2, add_unsecured_network/1,
         remove_network/1, remember_network/1, forget_network/1,
         list_networks/0]).
-import(os, [cmd/1]).
-import(re, [run/2, replace/4]).
-import(lists, [filter/2, map/2, sort/1, foldr/3, flatten/1]).
-define(SCAN_ERROR_OUTPUT, "wlan0     Interface doesn't support scanning : Network is down").
-define(NETWORKS_FILE, "/etc/cruxis/networks").
-define(NETWORKS_DIR, "/etc/cruxis/networks.d").

disconnect() ->
    os:cmd("pkill '(wpa_supplicant|dhcpcd)'").

connect_wep(Ssid, Key) ->
    disconnect(),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
        0 -> ok;
        _ -> {failed, {failed_to_connect, Return}}
    end.

connect_wpa(Ssid, Key) ->
    disconnect(),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Supplicant_pid = 
        os:cmd(io_lib:format(
            "bash -c \"{ wpa_supplicant -D wext -i wlan0 -c <(wpa_passphrase '~s' <<< '~s') &> /dev/null & };
                       echo -n $!;
                       disown\"", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
        0 -> ok;
        _ -> os:cmd(io_lib:format("kill ~s", [Supplicant_pid])),
             {failed, {failed_to_connect, Return}}
    end.

connect_wpa(Conf_file) ->
    disconnect(),
    os:cmd("ip link set wlan0 up"),
    Ssid = os:cmd(io_lib:format("sed -rne 's/^[[:blank:]]*ssid=(.*)$/\\1/gp' '~s'", [Conf_file])),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Supplicant_pid = 
        os:cmd(io_lib:format(
            "{ wpa_supplicant -D wext -i wlan0 -c '~s' &> /dev/null & };
             echo -n $!;
             disown", [Conf_file])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
        0 -> ok;
        _ -> os:cmd(io_lib:format("kill ~s", [Supplicant_pid])),
             {failed, {failed_to_connect, Return}}
    end.

connect_unsecured(Ssid) ->
    disconnect(),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
       0 -> ok;
       _ -> {failed, {failed_to_connect, Return}}
    end.

connect_known_network(Id) ->
    disconnect(),
    connect_by_network_info(get_network_info(Id)).

connect_by_network_info({_, wpa, Conf_file}) -> connect_wpa(Conf_file);
connect_by_network_info({Ssid, wep, Key}) -> connect_wep(Ssid, Key);
connect_by_network_info({Ssid, unsecured}) -> connect_unsecured(Ssid).

auto_connect() ->
    Ssids = scan_networks(),
    Remembered_networks = get_remembered_networks(),
    case lists:dropwhile(fun(Network) -> connect_if_visible(Network, Ssids) =/= ok end, Remembered_networks) of
        [] -> {failed, no_remembered_networks_present};
        _  -> ok
    end.

scan_networks() ->
    os:cmd("ip link set wlan0 up"),
    Scan_result = scan_times(10),
    os:cmd("ip link set wlan0 down"),
    Scan_result.

scan_times(N) ->
    case N of 
        0 -> exit({error, unable_to_scan});
        _ -> case os:cmd("iwlist wlan0 scan") of
                ?SCAN_ERROR_OUTPUT -> scan_times(N-1);
                Scan_output        -> filter_scan(Scan_output)
             end
    end.

filter_scan(Scan_output) ->
    Lines = string:tokens(Scan_output, "\n"),
    {ok, Regex1} = re:compile("^[[:blank:]]*ESSID:"),
    Ssid_lines = lists:filter(fun(Line) -> re:run(Line, Regex1) =/= nomatch end, Lines),
    {ok, Regex2} = re:compile("^[[:blank:]]*ESSID:\"(.*)\"$"),
    lists:map(fun(Ssid_line) -> re:replace(Ssid_line, Regex2, "\\1", [{return, list}]) end, Ssid_lines).

drop_newline(Str) ->
    lists:foldr(fun(Char, Lst) ->
                    if Char =:= 10 andalso Lst =:= [] -> [];
                       true                           -> [Char | Lst ]
                    end
                end, [], Str).

read_file_string(File) ->
    case file:read_file(File) of
        {error, Reason}       -> exit({error, {unable_to_read_file, File, Reason}});
        {ok, Binary_contents} -> drop_newline(binary_to_list(Binary_contents))
    end.

get_remembered_networks() ->
    Networks = lists:map(fun list_to_integer/1, string:tokens(read_file_string(?NETWORKS_FILE), "\n")),
    map(fun get_network_info/1, Networks).

get_network_info(N) ->
    List_output = file:list_dir(io_lib:format("~s/~B", [?NETWORKS_DIR, N])),
    case List_output of
        {ok, Info} ->
            case lists:sort(Info) of
                ["key", "ssid"] ->
                    {read_file_string(io_lib:format("~s/~B/ssid", [?NETWORKS_DIR, N])),
                     wep,
                     read_file_string(io_lib:format("~s/~B/key", [?NETWORKS_DIR, N]))};
                ["ssid", "wpa_supplicant.conf"] ->
                    {read_file_string(io_lib:format("~s/~B/ssid", [?NETWORKS_DIR, N])),
                     wpa,
                     lists:flatten(io_lib:format("~s/~B/wpa_supplicant.conf", [?NETWORKS_DIR, N]))};
                ["key"] ->
                    {read_file_string(io_lib:format("~s/~B/ssid", [?NETWORKS_DIR, N])),
                     unsecured}
            end;
        _ -> exit({error, {network_not_found, N}})
    end.

connect_if_visible(Network_info, Ssids) ->
    Ssid = element(1, Network_info),
    case lists:any(fun(X) -> X =:= Ssid end, Ssids) of
       false -> {failed, network_not_present};
       true  -> case connect_by_network_info(Network_info) of
                    ok -> ok;
                    Failed_condition -> Failed_condition
                end
    end.

get_next_network() ->
    {ok, Lst} = file:list_dir(?NETWORKS_DIR),
    Networks = lists:sort(lists:map(fun(X) -> erlang:list_to_integer(X) end, Lst)),
    least_not_in(Networks,0).

least_not_in([X | Xs],N) ->
    if X =/= N -> N;
       true    -> least_not_in(Xs, N+1)
    end;
least_not_in([], N) ->
    N.

make_network(Network_id, Ssid) ->
    case file:make_dir(io_lib:format("~s/~B", [?NETWORKS_DIR, Network_id])) of
        ok -> ok;
        {error, Reason1} -> exit({error, {unable_to_make_directory, Reason1}})
    end,
    case file:write_file(io_lib:format("~s/~B/ssid", [?NETWORKS_DIR, Network_id]), Ssid) of
        ok -> ok;
        {error, Reason2} -> exit({error, {unable_to_make_ssid_file, Reason2}})
    end.

new_network(Ssid) ->
    New_id = get_next_network(),
    make_network(New_id, Ssid),
    New_id.

add_wpa_network(Conf_file) ->
    Ssid = os:cmd(io_lib:format("sed -rne 's/^[[:blank:]]*ssid=\"(.*)\"$/\\1/gp' '~s'", [Conf_file])),
    Network_id = new_network(Ssid),
    case file:copy(Conf_file, io_lib:format("~s/~B/wpa_supplicant.conf",
                                            [?NETWORKS_DIR, Network_id])) of
        {ok, _} -> ok;
        {error, Reason1} -> exit({error, {unable_to_copy_conf_file, Reason1}})
    end,
    case file:change_mode(io_lib:format("~s/~B/wpa_supplicant.conf", [?NETWORKS_DIR, Network_id]), 384) of
        ok -> ok;
        {error, Reason2} -> exit({error, {unable_to_change_mode, Reason2}})
    end.

add_wpa_network(Ssid, Key) ->
    Network_id = new_network(Ssid),
    Conf_file_contents = os:cmd(io_lib:format("wpa_passphrase '~s' <<< '~s' 2> /dev/null",
                                              [Ssid, Key])),
    case file:write_file(io_lib:format("~s/~B/wpa_supplicant.conf", [?NETWORKS_DIR, Network_id]),
                         Conf_file_contents) of
        ok -> ok;
        {error, Reason1} -> exit({error, {unable_to_make_conf_file, Reason1}})
    end,
    case file:change_mode(io_lib:format("~s/~B/wpa_supplicant.conf", [?NETWORKS_DIR, Network_id]), 384) of
        ok -> ok;
        {error, Reason2} -> exit({error, {unable_to_change_mode, Reason2}})
    end.

add_wep_network(Ssid, Key) ->
    Network_id = new_network(Ssid),
    case file:write_file(io_lib:format("~s/~B/key", [?NETWORKS_DIR, Network_id]), Key) of
        ok -> ok;
        {error, Reason1} -> exit({error, {unable_to_make_key_file, Reason1}})
    end,
    case file:change_mode(io_lib:format("~s/~B/key", [?NETWORKS_DIR, Network_id]), 384) of
        ok -> ok;
        {error, Reason2} -> exit({error, {unable_to_change_mode, Reason2}})
    end.

add_unsecured_network(Ssid) ->
    new_network(Ssid).

remove_network(Network_id) ->
    lists:map(
        fun(File) -> 
            case file:delete(io_lib:format("~s/~B/~s", [?NETWORKS_DIR, Network_id, File])) of
                ok -> ok;
                {error, Reason} -> exit({error, {unable_to_delete_network_file, File, Reason}})
            end
        end,
        erlang:element(2,file:list_dir(io_lib:format("~s/~B", [?NETWORKS_DIR, Network_id])))),
    case file:del_dir(io_lib:format("~s/~B/", [?NETWORKS_DIR, Network_id])) of
        ok -> ok;
        {error, Reason} -> exit({error, {unable_to_delete_network, Reason}})
    end,
    Return = os:cmd(io_lib:format("sed -i -r -e '/^~B$/d' '~s' &> /dev/null; echo -n $?", [Network_id, ?NETWORKS_FILE])),
    case erlang:list_to_integer(Return) of
        0 -> ok;
        N -> exit({error, {unable_to_modify_networks_file, N}})
    end.

remember_network(Network_id) ->
    {ok, Networks} = file:list_dir(?NETWORKS_DIR),
    case lists:member(erlang:integer_to_list(Network_id), Networks) of
        true -> ok;
        false -> exit({error, network_not_found})
    end,
    Return = os:cmd(io_lib:format("egrep '~B' '~s'; echo -n $?", [Network_id, ?NETWORKS_FILE])),
    case erlang:list_to_integer(Return) of
        0 -> ok;
        _ -> case file:write_file(?NETWORKS_FILE, io_lib:format("~B~n", [Network_id]), [append]) of
                 ok -> ok;
                 {error, Reason} -> exit({error, {unable_to_write_to_networks_file, Reason}})
             end
    end.

forget_network(Network_id) ->
    Return = os:cmd(io_lib:format("sed -i -r -e '/^~B$/d' '~s' &> /dev/null; echo -n $?",
                                  [Network_id, ?NETWORKS_FILE])),
    case erlang:list_to_integer(Return) of
        0 -> ok;
        N -> exit({error, {unable_to_modify_networks_file, N}})
    end.

list_networks() ->
    {ok, Networks} = file:list_dir(?NETWORKS_DIR),
    Networks_with_info =
        lists:map(fun(Network) ->
                      {erlang:list_to_integer(Network),
                       read_file_string(io_lib:format("~s/~s/ssid", [?NETWORKS_DIR, Network]))}
            end,
            Networks),
    lists:sort(fun({N, _}, {M, _}) -> N =< M end, Networks_with_info).
