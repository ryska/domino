-module(dom_server).
-export([start/1, stop/1, add_func/2, send_to/2]).
%%%-----------------------------------------------------------------------------
%%% Glowny serwer aplikacji.
%%% Zajmuje sie wymiana danych pomiedzy klientami oraz wykonywaniem odpowiednich
%%% funkcji w zaleznosci od otrzymanych danych.
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Funkcja: start/1
%% Cel: Uruchamia serwer na podanym porcie oraz tworzy wszelkie potrzebne magazyny
%%     danych. Jesli podany port jest juz zajety to zwracany jest natychmiast blad.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
start(Port) ->
    try
        ets:new(dom_pids, [set, named_table]),
        PID = spawn(dom_server, read, [Port]),
        ets:insert(dom_pids, {read, Port, PID}),
        ets:new(dom_clients, [set, public, named_table]),
        ets:new(dom_data, [set, public,  named_table]),
        ets:new(dom_func, [bag, public,  named_table]),
        io:format("Uruchamiam serwer na porcie ~p...~n", [Port]),

        add_func(temp, fun dom_func:temp/1),
        add_func(dym, fun dom_func:dym/1),
        add_func(alarm, fun dom_func:alarm/1),

        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden serwer!~n", []),
        error
    end.

%%------------------------------------------------------------------------------
%% Funkcja: start/1
%% Cel: Zatrzymuje serwer dzialajacy na podanym porcie oraz usuwa wszelkie magazyny
%%     danych. Jesli nie istnieje serwer na podanym porcie to zwracany jest natychmiast blad.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
stop(Port) ->
    try
        PID = hd(hd(ets:match(dom_pids, {read, Port, '$1'}))),
        exit(PID, stop),
        ets:match_delete(dom_pids, {read, Port, '_'}),
        ets:delete(dom_pids),
        ets:delete(dom_clients),
        ets:delete(dom_data),
        ets:delete(dom_func),
        io:format("Zatrzymano serwer!~n"),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego serwera na porcie ~p!~n", [Port]),
        error
    end.

%%------------------------------------------------------------------------------
%% Funkcja: read/1
%% Cel: Odczytuje dane przychodzace do serwera a nastepnie reaguje asynchronicznie
%%     w zaleznosci od otrzymanych danych.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
read(Port) ->
    case dom_net:read(Port) of
        {error, _} ->
            io:format("Port ~p jest juz zajety przez inny proces!~n", [Port]),
            stop(Port);
        {ClientAddress, _, Data} ->
            spawn(dom_server, act, [ClientAddress, Data]),
            read(Port);
        _ ->
            stop(Port)
    end.

%%------------------------------------------------------------------------------
%% Funkcja: act/3
%% Cel: Reaguje na otrzymane dane w zaleznosci od ich formatu.
%% Argumenty: Adres klienta, krotka z danymi.
%%------------------------------------------------------------------------------
act(ClientAddress, {register, Id, Name, ClientPort}) ->
    ets:insert(dom_clients, {Id, Name, ClientAddress, ClientPort}),
    io:format("Rejestruje klienta o id ~p i nazwie ~p.~n", [Id, Name]);
act(_, {data, Id, Data}) ->
    ets:insert(dom_data, {Id, Data}),
    io:format("Otrzymalem dane od ID ~p: ~p~n", [Id, Data]),
    exec_func(Id);
act(_, {delete, Id}) ->
    try
        ets:delete(dom_clients, Id),
        io:format("Usuwam klienta o id ~p.~n", [Id])
    catch
        error:badarg -> io:format("Brak dzialajacego klienta o id ~p!~n", [Id])
    end.


%%------------------------------------------------------------------------------
%% Funkcja: get_data/1
%% Cel: Zwraca dane, ktore przyszly od clienta z podanym ID.
%% Argumenty: ID klienta.
%% Zwraca: Zapisane dane lub nil, jesli nie odebrano danych od klienta o podanym ID.
%%------------------------------------------------------------------------------
get_data(Id) ->
    case ets:lookup(dom_data, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

%%------------------------------------------------------------------------------
%% Funkcja: send_to/2
%% Cel: Wysyla podane dane do klienta o podanym ID.
%% Argumenty: ID klienta, dane do wyslania.
%%------------------------------------------------------------------------------
send_to(Id, Data) ->
    case ets:lookup(dom_clients, Id) of
        [] -> nil;
        [{Id, _, ClientAddress, ClientPort}] ->
            dom_net:send(ClientAddress, ClientPort, Data)
    end.

%%------------------------------------------------------------------------------
%% Funkcja: add_func/2
%% Cel: Dodaje funkcje reagujaca na otrzymanie danych od klienta o podanym ID.
%% Argumenty: ID klienta, funkcja reagujaca przyjmujaca jako argument otrzymane dane.
%%------------------------------------------------------------------------------
add_func(Id, Func) ->
    case is_function(Func) of
        true ->
            ets:insert(dom_func, {Id, Func});
        false ->
            io:format("Podany argument ~p nie jest funkcja!~n", [Func])
    end.

%%------------------------------------------------------------------------------
%% Funkcja: exec_func/1
%% Cel: Wywoluje wszystkie funkcje reagujace na otrzymanie danych od klienta
%%     o podanym ID.
%% Argumenty: ID klienta
%%------------------------------------------------------------------------------
exec_func(Id) ->
    case ets:lookup(dom_func, Id) of
        [] -> nil;
        Funcs ->
                lists:map(fun ({_, Func}) -> Func(get_data(Id)) end, Funcs)
    end.
