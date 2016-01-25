-module(dom_sms).
-export([start/4, stop/3, loop/3]).

%%%-------------------
%%% dom_sms symuluje zachowanie kontrolera wysyłania sms.
%%% funkcje: start, stop, loop
%%%-------------------

%%-------------------------
%% Funckja start
%% Rejestruje kontroler na serwerze,
%% uruchamia kontroler danym porcie.
%%-------------------------

start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam kontroler sms o id: ~p...~n", [Id]),
        PID = spawn(dom_sms, loop, [ServerAddress, ServerPort, Id]),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PID}),
        dom_client:register(ServerAddress, ServerPort, Id, Name, 8084),
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
        io:format("Zatrzymuje kontroler sms o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje aby wysłać sms o podanej treści.
%%-------------------------

loop(ServerAddress, ServerPort, Id) ->
    case dom_net:read(8084) of
        {_, _, Tresc} ->
            io:format("Wysylam sms: ~p ~n", [Tresc]);
        _ ->
            nil
    end,
    loop(ServerAddress, ServerPort, Id).
