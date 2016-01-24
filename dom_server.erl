-module(dom_server).
-compile(export_all).

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

get_data(Id) ->
    case ets:lookup(dom_data, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

send_to(Id, Data) ->
    case ets:lookup(dom_clients, Id) of
        [] -> nil;
        [{Id, _, ClientAddress, ClientPort}] ->
            dom_net:send(ClientAddress, ClientPort, Data)
    end.

add_func(Id, Func) ->
    case is_function(Func) of
        true ->
            ets:insert(dom_func, {Id, Func});
        false ->
            io:format("Podany argument ~p nie jest funkcja!~n", [Func])
    end.

exec_func(Id) ->
    case ets:lookup(dom_func, Id) of
        [] -> nil;
        Funcs ->
                lists:map(fun ({_, Func}) -> Func(get_data(Id)) end, Funcs)
    end.

% fun
%
% end.
