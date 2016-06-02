-module(shutdown_handler).
-behavior(lasse_handler).

-dialyzer(no_behaviours).

-export([
         init/3,
         handle_info/2,
         handle_notify/2,
         handle_error/3,
         terminate/3
        ]).

-record(state,
        {
          module :: module(),
          state :: any()
        }).



init(_InitArgs, _LastEventId, Req) ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    Body = <<"Sorry, shutdown!">>,
    {shutdown, 404, Headers, Body, Req, #state{}}.

handle_info(Msg, State) ->
    {send, [{data, Msg}], State}.

handle_notify(Msg, State) ->
    {send, [{data, Msg}], State}.

handle_error(_, _, _) ->
    doesnt_matter.

terminate(_, _, _) ->
    ok.
