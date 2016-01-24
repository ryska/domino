-module(dom_client).
-compile(export_all).

register(ServerAddress, ServerPort, Id, Name, ClientPort) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, ServerAddress, ServerPort, term_to_binary({register, Id, Name, ClientPort})).

data(ServerAddress, ServerPort, Id, Data) ->
    dom_net:send(ServerAddress, ServerPort, {data, Id, Data}).

delete(ServerAddress, ServerPort, Id) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, ServerAddress, ServerPort, term_to_binary({delete, Id})).
