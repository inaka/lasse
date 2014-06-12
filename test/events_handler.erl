-module(events_handler).
-behavior(lasse_handler).

-export([
         init/2,
         handle_info/2,
         handle_notify/2
        ]).

init(_InitArgs, Req) ->
    lager:info("Initiating the handler."),
    erlang:send_after(10, self(), {message, <<"notify chunk">>}),
    erlang:send_after(20, self(), <<"info chunk">>),
    {ok, Req, {}}.

handle_info(Msg, State) ->
    {send, [{data, Msg}], State}.

handle_notify(Msg, State) ->
    {send, [{data, Msg}], State}.
