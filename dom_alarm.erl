-module(dom_alarm).
-export([start/4, stop/3, loop/3]).

%%%-------------------
%%% dom_alarm symuluje zachowanie czujnika alarmu.
%%% funkcje: start, stop, loop
%%%-------------------

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik alarmu danym porcie.
%%-------------------------

start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam czujnik alarmu o id: ~p...~n", [Id]),
        PID = spawn(dom_alarm, loop, [ServerAddress, ServerPort, Id]),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PID}),
        dom_client:register(ServerAddress, ServerPort, Id, Name, 0),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika alarmu.
%%-------------------------

stop(ServerAddress, ServerPort, Id) ->
    try
        dom_client:delete(ServerAddress, ServerPort, Id),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik alarmu o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Wysyła co 10 sekund do serwera informacje, czy doszło do włamania.
%%-------------------------

loop(ServerAddress, ServerPort, Id) ->
    case random:uniform(1) of
        1 ->
            dom_client:data(ServerAddress, ServerPort, Id, tak);
        _ ->
        dom_client:data(ServerAddress, ServerPort, Id, nie)
    end,
    timer:sleep(timer:seconds(10)),
    loop(ServerAddress, ServerPort, Id).
