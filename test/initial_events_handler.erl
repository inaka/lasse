-module(initial_events_handler).
-behavior(lasse_handler).

-dialyzer(no_behaviours).

-export([
         init/3,
         handle_info/2,
         handle_notify/2,
         handle_error/3,
         terminate/3
        ]).

init(_InitArgs, _LastEventId, Req) ->
  InitialEvents = [#{data => <<"initial 1">>}, #{data => <<"initial 2">>}],
  {ok, Req, InitialEvents, {}}.

handle_info(_, _) ->
  does_not_matter.

handle_notify(_, _) ->
  does_not_matter.

handle_error(_, _, _) ->
  doesnt_matter.

terminate(_, _, _) ->
  ok.
