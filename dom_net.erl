-module(dom_net).
-export([read/1, read/2, send/3]).
%%%-----------------------------------------------------------------------------
%%% Funkcje pomocnicze do wygodnej komunikacji przez protokol UDP.
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Funkcja: read/2
%% Cel: Odczytuje przychodzace dane na protokole UDP i podanym porcie czekajac
%%     podana ilosc milisekund.
%% Argumenty: Port - numer portu, Timeout - ilosc milisekund czekania.
%% Zwraca: Krotka w formacie {SenderAddress, SenderPort, Data} lub {}
%%     jesli wystapil blad lub nie odebrano danych.
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
%% Funkcja: read/1
%% Cel: Odczytuje przychodzace dane na protokole UDP i podanym porcie czekajac
%%     1000 milisekund.
%% Argumenty: Port - numer portu.
%% Zwraca: Krotka w formacie {SenderAddress, SenderPort, Data} lub {}
%%     jesli wystapil blad lub nie odebrano danych.
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
%% Funkcja: send/3
%% Cel: Wysyla dane przez protokol UDP na podany adress i port.
%% Argumenty: Adres docelowy jako krotka, port oraz dane do wyslania.
%%------------------------------------------------------------------------------
send(Address, Port, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Address, Port, term_to_binary(Data)).
