-module(ping_handler).
-export([
         init/3,
         handle/2,
         info/3,
         terminate/3
        ]).

init(_Transport, Req, _Opts) ->
    {ok, Req, {}}.

handle(Req, State) ->
    lists:foreach(fun ping/1, pg2:get_members(pongers)),
    cowboy_req:reply(200, [], <<"ack">>, Req),
    {ok, Req, State}.

info(_Msg, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

ping(Pid) ->
    lasse_handler:notify(Pid, ping).
