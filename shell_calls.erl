-module(shell_calls).
%-export([connect_wep/2, connect_wpa/1, connect_wpa/2, connect_unsecured/1,
         %connect_known_network/1, auto_connect/0]).
-compile([export_all]).
-import(os, [cmd/1]).
-import(re, [run/2, replace/4]).
-import(lists, [filter/2, map/2, sort/1, foldr/3, flatten/1]).
-define(SCAN_ERROR_OUTPUT, "wlan0     Interface doesn't support scanning : Network is down").
-define(NETWORKS_FILE, "/etc/cruxis/networks").
-define(NETWORKS_DIR, "/etc/cruxis/networks.d").

connect_wep(Ssid, Key) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
        0 -> ok;
        _ -> {error, Return}
    end.

connect_wpa(Ssid, Key) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Supplicant_pid = 
        os:cmd(io_lib:format(
            "zsh -c \"{ wpa_supplicant -D wext -i wlan0 -c <(wpa_passphrase '~s' <<< '~s') &> /dev/null &| };
             echo -n $!\"", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
        0 -> ok;
        _ -> os:cmd(io_lib:format("kill ~s", [Supplicant_pid])),
             {error, Return}
    end.

connect_wpa(Conf_file) ->
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
                       {error, Return}
    end.

connect_unsecured(Ssid) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    case Return of
       0 -> ok;
       _ -> {error, Return}
    end.

connect_known_network(Id) ->
    connect_by_network_info(get_network_info(Id)).

connect_by_network_info({_, wpa, Conf_file}) -> connect_wpa(Conf_file);
connect_by_network_info({Ssid, wep, Key}) -> connect_wep(Ssid, Key);
connect_by_network_info({Ssid, unsecured}) -> connect_unsecured(Ssid).

auto_connect() ->
    Ssids = scan_networks(),
    Known_networks = get_known_networks(),
    case lists:dropwhile(fun(Network) -> connect_if_visible(Network, Ssids) =/= ok end, Known_networks) of
        [] -> {error, no_known_networks_present};
        _  -> ok
    end.

scan_networks() ->
    os:cmd("ip link set wlan0 up"),
    Scan_result = scan_times(10),
    os:cmd("ip link set wlan0 down"),
    Scan_result.

scan_times(N) ->
    if N =:= 0 -> {error, unable_to_scan};
       true    -> Scan_output = os:cmd("iwlist wlan0 scan"),
                  if Scan_output =:= ?SCAN_ERROR_OUTPUT -> scan_times(N-1);
                     true                               -> filter_scan(Scan_output)
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
    {ok, Binary_contents} = file:read_file(File),
    drop_newline(binary_to_list(Binary_contents)).

get_known_networks() ->
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
        _ -> {error, network_not_found}
    end.

connect_if_visible(Network_info, Ssids) ->
    Ssid = element(1, Network_info),
    case lists:any(fun(X) -> X =:= Ssid end, Ssids) of
       false -> failed;
       true  -> case connect_by_network_info(Network_info) of
                    ok -> ok;
                    _ -> failed
                end
    end.
