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

-spec init(term(), term(), cowboy_req:req()) ->
    {shutdown, 404, cowboy:http_headers(), binary(), cowboy_req:req(), {}}.
init(_InitArgs, _LastEventId, Req) ->
  Headers = #{<<"content-type">> => <<"text/html">>},
  Body = <<"Sorry, shutdown!">>,
  {shutdown, 404, Headers, Body, Req, {}}.

-spec handle_info(term(), term()) -> {send, [{data, term()}], term()}.
handle_info(Msg, State) ->
  {send, [{data, Msg}], State}.

-spec handle_notify(term(), term()) -> {send, [{data, term()}], term()}.
handle_notify(Msg, State) ->
  {send, [{data, Msg}], State}.

-spec handle_error(term(), term(), term()) -> doesnt_matter.
handle_error(_, _, _) ->
  doesnt_matter.

-spec terminate(term(), term(), term()) -> ok.
terminate(_, _, _) ->
  ok.
