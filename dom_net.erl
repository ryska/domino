-module(dom_net).
-export([read/1, read/2, send/3]).

%%------------------------------------------------------------------------------
%% Function: read/2
%% Purpose: Reads incoming UDP data with timeout.
%% Args: Port number to open for reading and Timeout in miliseconds.
%% Returns: Touple in format {SenderAddress, SenderPort, Data} or {}
%%     if errror occured.
%%------------------------------------------------------------------------------
read(Port, Timeout) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            Return = recv(Socket, Timeout);
        {error, eaddrinuse} ->
            io:format("Port ~p jest już zajęty przez inny proces!~n", [Port]),
            Return = {error, eaddrinuse};
        {error, Reason} ->
            Return = {error, Reason}
    end,
    Return.

%%------------------------------------------------------------------------------
%% Function: read/1
%% Purpose: Reads incoming UDP data with default timeout of 1000 miliseconds.
%% Args: Port number to open for reading.
%% Returns: Touple in format {SenderAddress, SenderPort, Data} or {}
%%     if errror occured.
%%------------------------------------------------------------------------------
read(Port) ->
    read(Port, 1000).

%%------------------------------------------------------------------------------
%% Function: recv/2
%% Purpose: Reads UDP data from given Socket with given Timeout.
%% Args: Connection Socket and Timeout in miliseconds.
%% Returns: Touple in format {SenderAddress, SenderPort, Data} or {error, Reason}
%%     if errror occured or {}.
%%------------------------------------------------------------------------------
recv(Socket, Timeout) ->
    case gen_udp:recv(Socket, 0, Timeout) of
        {ok, {Address, Port, Packet}} ->
            Return = {Address, Port, binary_to_term(Packet)};
        {error, Reason} ->
            io:format("Błąd: ~p~n", [Reason]),
            Return = {error, Reason}
    end,
    gen_tcp:close(Socket),
    Return.

%%------------------------------------------------------------------------------
%% Function: send/3
%% Purpose: Send data with UDP protocol to given address and port.
%% Args: Address of reciever as a touple, Port number and Data in any type.
%%------------------------------------------------------------------------------
send(Address, Port, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Address, Port, term_to_binary(Data)).
