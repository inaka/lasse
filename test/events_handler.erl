-module(events_handler).
-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).

-type event() ::
    #{ id => binary()
     , event => binary()
     , data => binary()
     , retry => binary()
     , comment | '' => binary()
     }.

-type result() ::
  {'send', Event :: event(), NewState :: any()} |
  {'nosend', NewState :: any()} |
  {'stop', NewState :: any()}.

-spec init(any(), undefined | binary(), cowboy_req:req()) ->
  {ok, cowboy_req:req(), undefined | binary()}.
init(_InitArgs, LastEventId, Req) ->
  % Take process name from the "process-name" header.
  case cowboy_req:header(<<"process-name">>, Req) of
    undefined -> ok;
    ProcNameBin ->
      ProcName = binary_to_atom(ProcNameBin, utf8),
      register(ProcName, self())
  end,
  {ok, Req, LastEventId}.

-spec handle_notify(send | send_id | no_data | nosend | comments | last_event_id | stop, any()) ->
  result().
handle_notify(send, State) ->
  {send, #{data => <<"notify chunk">>}, State};
handle_notify(send_id, State) ->
  Event = #{ id => <<"1">>
           , data => <<"notify chunk">>
           , event => <<"">>
           },
  {send, Event, State};
handle_notify(no_data, State) ->
  Event = #{ id => <<"1">>
           , event => "no_data"
           },
  {send, Event, State};
handle_notify(nosend, State) ->
  {nosend, State};
handle_notify(comments, State) ->
  Event = #{ comment => <<"Comment 1\nComment 2">>
           , data => <<"some data">>
           },
  {send, Event, State};
handle_notify(last_event_id, State) ->
  Event = #{data => State},
  {send, Event, State};
handle_notify(stop, State) ->
  {stop, State}.

-spec handle_info(send | nosend | stop, any()) -> result().
handle_info(send, State) ->
  {send, #{data => <<"info chunk">>}, State};
handle_info(nosend, State) ->
  {nosend, State};
handle_info(stop, State) ->
  {stop, State}.

-spec handle_error(any(), any(), any()) -> any().
handle_error(_Msg, _Reason, State) ->
  State.

-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
  ok.
