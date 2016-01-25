-module(dom_dym).
-export([start/4, stop/3, loop/3]).

%%%-------------------
%%% dom_dym symuluje zachowanie czujnika dymu.
%%% funkcje: start, stop, loop
%%%-------------------

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik dymu na danym porcie.
%%-------------------------
start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam czujnik dymu o id: ~p...~n", [Id]),
        PID = spawn(dom_dym, loop, [ServerAddress, ServerPort, Id]),
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
%% Zatrzymuje działanie czujnika dymu.
%%-------------------------

stop(ServerAddress, ServerPort, Id) ->
    try
        dom_client:delete(ServerAddress, ServerPort, Id),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik dymu o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 10 sekund do serwera informacje o obecności dymu.
%%-------------------------

    loop(ServerAddress, ServerPort, Id) ->
         %-- random mdz 0 i 1
        case random:uniform(2) of
            1 ->
                dom_client:data(ServerAddress, ServerPort, Id, tak);
            _ ->
                dom_client:data(ServerAddress, ServerPort, Id, nie)
        end,
        timer:sleep(timer:seconds(10)),
        loop(ServerAddress, ServerPort, Id).
