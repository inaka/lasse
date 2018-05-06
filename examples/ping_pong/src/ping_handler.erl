-module(ping_handler).
-export([
         init/2,
         handle/2,
         info/3,
         terminate/3
        ]).

init(Req, _Opts) ->
  {ok, Req, {}}.

handle(Req, State) ->
  lists:foreach(fun ping/1, pg2:get_members(pongers)),
  Req1 = cowboy_req:reply(200, #{}, <<"ack">>, Req),
  {ok, Req1, State}.

info(_Msg, Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

ping(Pid) ->
  lasse_handler:notify(Pid, ping).
