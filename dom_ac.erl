-module(dom_ac).
-export([start/4, stop/3, loop/3]).

start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam kontroler klimatyzacji o id: ~p...~n", [Id]),
        PID = spawn(dom_ac, loop, [ServerAddress, ServerPort, Id]),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PID}),
        dom_client:register(ServerAddress, ServerPort, Id, Name, 8081),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

stop(ServerAddress, ServerPort, Id) ->
    try
        dom_client:delete(ServerAddress, ServerPort, Id),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje kontroler klimatyzacji o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

loop(ServerAddress, ServerPort, Id) ->
    case dom_net:read(8081) of
        {_, _, wlacz} ->
            io:format("Wlaczam klimatyzacje ~n");
        {_, _, wylacz} ->
            io:format("Wylaczam klimatyzacje ~n");
        _ ->
            nil
    end,
    loop(ServerAddress, ServerPort, Id).
