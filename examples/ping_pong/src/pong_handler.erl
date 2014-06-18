-module(pong_handler).
-behavior(lasse_handler).

-export([
         init/3,
         handle_notify/2,
         handle_info/2,
         handle_error/3,
         terminate/3
        ]).

init(_InitArgs, _LastEventId, Req) ->
    pg2:join(pongers, self()),
    {ok, Req, {}}.

handle_notify(ping, State) ->
    {send, [{data, <<"pong">>}], State}.

handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reason, State) ->
    State.

terminate(_Reason, _Req, _State) ->
    ok.
