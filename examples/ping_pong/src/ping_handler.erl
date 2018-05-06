-module(ping_handler).
-export([init/2]).

init(Req, _Opts) ->
  io:format("Pinging ~p~n", [pg2:get_members(pongers)]),
  lists:foreach(fun ping/1, pg2:get_members(pongers)),
  Req1 = cowboy_req:reply(200, #{}, <<"ack">>, Req),
  {ok, Req1, nostate}.

ping(Pid) ->
  lasse_handler:notify(Pid, ping).
