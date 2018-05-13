-module(dummy_handler).
-behavior(lasse_handler).

-dialyzer(no_behaviours).

-export([ init/3
        , handle_info/2
        , handle_notify/2
        , handle_error/3
        , terminate/3
        ]).

-spec init(any(), undefined | binary(), cowboy_req:req()) -> {ok, cowboy_req:req(), {}}.
init(_InitArgs, _LastEventId, Req) ->
  {ok, Req, {}}.

-spec handle_info(any(), any()) -> does_not_matter.
handle_info(_, _) ->
  does_not_matter.

-spec handle_notify(any(), any()) -> does_not_matter.
handle_notify(_, _) ->
  does_not_matter.

-spec handle_error(any(), any(), any()) -> doesnt_matter.
handle_error(_, _, _) ->
  doesnt_matter.

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_, _, _) ->
  ok.
