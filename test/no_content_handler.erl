-module(no_content_handler).
-behavior(lasse_handler).

-dialyzer(no_behaviours).

-export([
         init/3,
         handle_info/2,
         handle_notify/2,
         handle_error/3,
         terminate/3
        ]).

-spec init(term(), term(), cowboy_req:req()) ->
    {no_content, cowboy_req:req(), {}}.
init(_InitArgs, _LastEventId, Req) ->
    {no_content, Req, {}}.

-spec handle_info(term(), term()) -> does_not_matter.
handle_info(_, _) ->
    does_not_matter.

-spec handle_notify(term(), term()) -> does_not_matter.
handle_notify(_, _) ->
    does_not_matter.

-spec handle_error(term(), term(), term()) -> doesnt_matter.
handle_error(_, _, _) ->
    doesnt_matter.

-spec terminate(term(), term(), term()) -> ok.
terminate(_, _, _) ->
    ok.
