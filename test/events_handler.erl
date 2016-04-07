-module(events_handler).
-behavior(lasse_handler).

-export([
         init/2,
         handle_notify/2,
         handle_info/2,
         handle_error/3,
         terminate/3,
         upgrade/6
        ]).


-spec upgrade(Req, Env, module(), any(), timeout(), run | hibernate)
  -> {ok, Req, Env} | {suspend, module(), atom(), [any()]}
  when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, Timeout, run) ->
  cowboy_loop:upgrade(Req, Env, Handler, HandlerState, Timeout, run);
upgrade(Req, Env, Handler, HandlerState, Timeout, hibernate) ->
  cowboy_loop:upgrade(Req, Env, Handler, HandlerState, Timeout, hibernate).

init(LastEventId, Req) ->
    % Take process name from the "process-name" header.
    case cowboy_req:header(<<"process-name">>, Req) of
        ProcNameBin when ProcNameBin =/= <<"undefined">> ->
            ProcName = binary_to_atom(ProcNameBin, utf8),
            register(ProcName, self());
        undefined  ->
            ok
    end,

    {ok, Req, LastEventId}.

handle_notify(send, State) ->
    {send, #{data => <<"notify chunk">>}, State};
handle_notify(send_id, State) ->
    Event = #{id => <<"1">>,
              data => <<"notify chunk">>,
              event => <<"">>
             },
    {send, Event, State};
handle_notify(no_data, State) ->
    Event = #{id => <<"1">>,
              event => "no_data"
             },
    {send, Event, State};
handle_notify(nosend, State) ->
    {nosend, State};
handle_notify(comments, State) ->
    Event = #{comment => <<"Comment 1\nComment 2">>,
              data => <<"some data">>
             },
    {send, Event, State};
handle_notify(last_event_id, State) ->
    Event = #{data => State},
    {send, Event, State};
handle_notify(stop, State) ->
    {stop, State}.

handle_info(send, State) ->
    {send, #{data => <<"info chunk">>}, State};
handle_info(nosend, State) ->
    {nosend, State};
handle_info(stop, State) ->
    {stop, State}.

handle_error(_Msg, _Reason, State) ->
    State.

terminate(_Reason, _Req, _State) ->
    ok.
