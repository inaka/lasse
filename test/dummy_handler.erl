-module(dummy_handler).
-behavior(lasse_handler).

-export([
         init/2,
         handle_info/2,
         handle_notify/2
        ]).

init(_InitArgs, Req) ->
    {ok, Req, {}}.

handle_info(_, _) ->
    does_not_matter.

handle_notify(_, _) ->
    does_not_matter.
