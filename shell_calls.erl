-module(shell_calls).
-export([connect_wep/2, connect_wpa/1, connect_wpa/2, connect_unsecured/1]).
-import(os).

connect_wep(Ssid, Key) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s' key '~s'", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    if Return =/= 0 -> {failed, Return};
       Return =:= 0 -> ok
    end.

connect_wpa(Ssid, Key) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Supplicant_pid = 
        os:cmd(io_lib:format(
            "zsh -c \"{ wpa_supplicant -D wext -i wlan0 -c <(wpa_passphrase '~s' <<< '~s') &> /dev/null &| };
             echo -n $!\"", [Ssid, Key])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    if Return =/= 0 -> os:cmd(io_lib:format("kill ~s", [Supplicant_pid])),
                       {failed, Return};
       Return =:= 0 -> ok
    end.

connect_wpa(ConfFile) ->
    os:cmd("ip link set wlan0 up"),
    Ssid = os:cmd(io_lib:format("sed -rne 's/^[[:blank:]]*ssid=(.*)$/\\1/gp' '~s'", [ConfFile])),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Supplicant_pid = 
        os:cmd(io_lib:format(
            "{ wpa_supplicant -D wext -i wlan0 -c '~s' &> /dev/null & };
             echo -n $!;
             disown", [ConfFile])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    if Return =/= 0 -> os:cmd(io_lib:format("kill ~s", [Supplicant_pid])),
                       {failed, Return};
       Return =:= 0 -> ok
    end.

connect_unsecured(Ssid) ->
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    os:cmd("ip link set wlan0 down"),
    os:cmd("ip link set wlan0 up"),
    os:cmd(io_lib:format("iwconfig wlan0 essid '~s'", [Ssid])),
    Return = list_to_integer(os:cmd("dhcpcd wlan0 &> /dev/null; echo -n $?")),
    if Return =/= 0 -> {failed, Return};
       Return =:= 0 -> ok
    end.

connect_known_network(Id) ->
    3.

auto_connect() ->
    Ssids = scan_networks(),
    KnownNetworks = get_known_networks(),
    if dropwhile(fun (Network) -> try_connect(Network, Ssids), KnownNetworks) =/= [] ->
                    ok;
       true -> {error, noKnownNetworksPresent}
    end.
