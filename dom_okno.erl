-module(dom_okno).
-export([start/4, stop/3, loop/3]).

%%%-------------------
%%% dom_okno symuluje zachowanie kontrolera wysyłania sms.
%%% funkcje: start, stop, loop
%%%-------------------

%%-------------------------
%% Funckja start
%% Rejestruje kontroler na serwerze,
%% uruchamia kontroler danym porcie.
%%-------------------------


start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam kontroler okien o id: ~p...~n", [Id]),
        PID = spawn(dom_okno, loop, [ServerAddress, ServerPort, Id]),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PID}),
        dom_client:register(ServerAddress, ServerPort, Id, Name, 8085),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie kontrolera.
%%-------------------------

stop(ServerAddress, ServerPort, Id) ->
    try
        dom_client:delete(ServerAddress, ServerPort, Id),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje kontroler okien o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje z informacją, czy otworzyć okna.
%%-------------------------

loop(ServerAddress, ServerPort, Id) ->
    case dom_net:read(8085) of
        {_, _, otworz} ->
            io:format("Otwieram okna ~n");
        {_, _, zamknij} ->
            io:format("Zamykam okna ~n");
        _ ->
            nil
    end,
    loop(ServerAddress, ServerPort, Id).
