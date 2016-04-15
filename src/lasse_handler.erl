%%% @doc Server-Sent Event handler for Cowboy
-module(lasse_handler).

-behaviour(cowboy_loop).

-export([
         init/2,
         info/3,
         terminate/3
        ]).

-export([
         notify/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state,
        {
          module :: module(),
          state :: any()
        }).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-export_type([event/0, result/0]).

-callback init(LastEvtId::undefined | binary(),
               Req::cowboy_req:req()) ->
  {ok, NewReq :: cowboy_req:req(), State :: any()} |
  {ok, NewReq :: cowboy_req:req(), Events :: [event()], State :: any()} |
  {no_content, NewReq :: cowboy_req:req(), State :: any()} |
  {
    shutdown,
    StatusCode :: cowboy:http_status(),
    Headers :: cowboy:http_headers(),
    Body :: iodata(),
    NewReq :: cowboy_req:req(),
    State :: any()
  }.

-callback handle_notify(Msg :: any(), State :: any()) ->
  result().

-callback handle_info(Msg :: any(), State :: any()) ->
  result().

-callback handle_error(Msg :: any(), Reason :: any(), State :: any()) ->
  any().

-callback terminate(
  Reason :: any(), Req :: cowboy_req:req(), State :: any()
) -> any().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type lasse_handler_options() ::
  module() |
  #{ module => module()
   , init_args => any()
   }.

-spec init(cowboy_req:req(), lasse_handler_options()) ->
  {ok | module(), any(), state()}.
init(Req, []) ->
    init(Req, #{});
init(Req, [Module]) when is_atom(Module) ->
    init(Req, #{module => Module});
init(Req, Opts) ->
  try
    #{module := Module} = Opts,
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req),
    InitResult = Module:init(LastEventId, Req),
    handle_init(InitResult, Module)
  catch
    _:{badmatch, #{}} ->
      throw(module_option_missing)
  end.

-spec info(term(), cowboy_req:req(), state()) ->
    {ok|stop, cowboy_req:req(), state()}.
info({message, Msg}, Req, State) ->
  Module = State#state.module,
  ModuleState = State#state.state,
  Result = Module:handle_notify(Msg, ModuleState),
  process_result(Result, Req, State);
info(Msg, Req, State) ->
  Module = State#state.module,
  ModuleState = State#state.state,
  Result = Module:handle_info(Msg, ModuleState),
  process_result(Result, Req, State).

-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(Reason, Req, State) ->
  Module = State#state.module,
  ModuleState = State#state.state,
  Module:terminate(Reason, Req, ModuleState),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec notify(atom() | pid(), term()) -> ok.
notify(Pid, Msg) ->
  Pid ! {message, Msg},
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_init({ok, Req, State}, Module) ->
  handle_init({ok, Req, [], State}, Module);
handle_init({ok, Req, InitialEvents, State}, Module) ->
  case cowboy_req:method(Req) of
    <<"GET">> ->
      % "no-cache recommended to prevent caching of event data.
      Headers = [{<<"content-type">>, <<"text/event-stream">>},
                 {<<"cache-control">>, <<"no-cache">>}],
      Req1 = cowboy_req:chunked_reply(200, Headers, Req),
      lists:foreach(
        fun(Event) -> ok = send_event(Event, Req1) end,
        InitialEvents
      ),
      {cowboy_loop, Req1, #state{module = Module, state = State}};

   _OtherMethod ->
      Headers = [{<<"content-type">>, <<"text/html">>}],
      StatusCode = 405, % Method not Allowed
      Req1 = cowboy_req:reply(StatusCode, Headers, Req),
      {cowboy_loop, Req1, #state{module = Module}}
  end;
handle_init({no_content, Req, State}, Module) ->
  Req1 = cowboy_req:reply(204, [], Req),
  {ok, Req1, #state{module = Module, state = State}};
handle_init({shutdown, StatusCode, Headers, Body, Req, State}, Module) ->
  Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
  {ok, Req1, #state{module = Module, state = State}}.

process_result({send, Event, NewState}, Req, State) ->
  try send_event(Event, Req) of
    ok -> {ok, Req, State#state{state = NewState}}
  catch
    Reason ->
      Module = State#state.module,
      ModuleState = State#state.state,
      ErrorNewState = Module:handle_error(Event, Reason, ModuleState),
      {ok, Req, ErrorNewState}
  end;
process_result({nosend, NewState}, Req, State) ->
  {ok, Req, State#state{state = NewState}};
process_result({stop, NewState}, Req, State) ->
  {stop, Req, State#state{state = NewState}}.

send_event(Event, Req) ->
  EventMsg = build_event(Event),
  cowboy_req:chunk(EventMsg, Req).

build_event(Event) ->
  [build_comment(maps:get(comment, Event, undefined)),
   build_comment(maps:get('', Event, undefined)),
   build_field(<<"id: ">>, maps:get(id, Event, undefined)),
   build_field(<<"event: ">>, maps:get(event, Event, undefined)),
   build_data(maps:get(data, Event, undefined)),
   build_field(<<"retry: ">>, maps:get(retry, Event, undefined)),
   <<"\n">>].

build_comment(undefined) ->
  [];
build_comment(Comment) ->
  [[<<": ">>, X, <<"\n">>] || X <- binary:split(Comment, <<"\n">>, [global])].

build_field(_, undefined) ->
  [];
build_field(Name, Value) ->
  [Name, Value, <<"\n">>].

build_data(undefined) ->
  throw(data_required);
build_data(Data) ->
  [[<<"data: ">>, X, <<"\n">>]
  || X <- binary:split(Data, <<"\n">>, [global])].
