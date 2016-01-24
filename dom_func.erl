-module(dom_func).
-compile(export_all).

temp(nil) -> nil;
temp(Data) when Data > 28 ->
    io:format("Temperatura wynosi ~p, wlaczam klimatyzacje...~n", [Data]),
    dom_server:send_to(klim, wlacz);
temp(_)  ->
    dom_server:send_to(klim, wylacz).



alarm(tak) ->
    io:format("Ktos wlamuje sie do domu, alarm!~n"),
    dom_server:send_to(sms, "Ktos wlamuje sie do domu, alarm!");
alarm(_) -> nil.

dym(tak) ->
    io:format("Czujnik wykryl dym!~n"),
    dom_server:send_to(sms, "Czujnik wykryl dym!"),
    io:format("Otwieram okna..."),
    dom_server:send_to(okna, otworz);
dym(_) ->
    dom_server:send_to(okna, zamknij).
