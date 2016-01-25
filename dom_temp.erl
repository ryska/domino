-module(dom_temp).
-export([start/4, stop/3, loop/3]).

%%%-------------------
%%% dom_temp symuluje zachowanie czujnika temperatury
%%% funkcje: start, stop, loop
%%%-------------------


%%-------------------------
%% funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik temperatury na danym porcie.
%%-------------------------
start(ServerAddress, ServerPort, Id, Name) ->
    try
        io:format("Uruchamiam czujnik temperatury o ID ~p...~n", [Id]),
        PID = spawn(dom_temp, loop, [ServerAddress, ServerPort, Id]),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PID}),
        dom_client:register(ServerAddress, ServerPort, Id, Name, 0),
        start.
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika temperatury.
%%-------------------------
stop(ServerAddress, ServerPort, Id) ->
    try
        dom_client:delete(ServerAddress, ServerPort, Id),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik temperatury o ID ~p...~n", [Id]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 5 sekund do serwera informacje o temperaturze.
%%-------------------------

loop(ServerAddress, ServerPort, Id) ->
    dom_client:data(ServerAddress, ServerPort, Id, random:uniform(40)),
    timer:sleep(timer:seconds(5)),
    loop(ServerAddress, ServerPort, Id).
