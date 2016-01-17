-module(dom_temp).
-compile(export_all).
start() ->
  % ets:new(pids,[set,named_table]),
  SendPID = spawn(dom_temp,fun() -> send({127,0,0,1}, 8853) end),
  LoopPID = spawn(dom_temp, fun() -> loop(SendPID) end).
  % ets:insert(pids,[{send, SendPID},{loop, LoopPID}]).

% stop() ->
  % [{_, LoopPID}] = ets:lookup(pids,loop),
  % [{_, SendPID}] = ets:lookup(pids,send),
  % LoopPID!stop,
  % SendPID!stop.


loop(SendPID) ->
  receive
    stop -> nil;
    generate ->
      timer:sleep(timer:seconds(1)),
      loop(SendPID),
      random:uniform(40)
  end.

send(Address, Port) ->
  receive
    stop -> nil;

    Socket ->
      {ok, Socket} = gen_udp:connect(Address, Port, [binary, {active, true}]),
      gen_udp:send(Socket, "Hej dziubasku"),
      send(Address, Port)
  end.
