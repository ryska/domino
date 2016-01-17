-module(dom_server).
-export([start/1, stop/1]).
-record(server_info, {pids = []}).

start(Port) ->
    ets:new(pids, [set, named_table]),
    spawn(fun () -> {ok, ListenSocket} = gen_udp:listen(Port, [{active, true}]),
                    loop(ListenSocket) end).

stop() ->



loop(ListenSocket) ->
    {ok, Connection} = gen_udp:accept(ListenSocket),
    ConnectionHandler = spawn(fun () -> read(Connection) end),
    gen_udp:controlling_process(Connection, ConnectionHandler),
    loop(ListenSocket).

read(Connection) ->
    case gen_udp:recv(Connection, 0) of
        {ok, Data} ->
            io:format("Dostałem dane: ~p~n", [Data]);
        {error, closed} ->
            nil
    end,
    gen_tcp:close(Connection).

% case gen_udp:open(
%     Option#udp_server_option.port,
%     Option#udp_server_option.option
%   ) of
%     {ok, Socket} ->
%       proc_lib:init_ack(Parent, {ok, self()}),
%       recv(
%         proplists:get_value(active, Option#udp_server_option.option),
%         Socket, Module, Option
%       );
%     {error, Reason} ->
%       exit({error, Reason})
%   end.
%
% response(Str) ->
%     B = iolist_to_binary(Str),
%     iolist_to_binary(
%       io_lib:fwrite(
%          "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
%          [size(B), B])).
