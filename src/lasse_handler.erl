%%% @doc Server-Sent Event handler for Cowboy
-module(lasse_handler).

-export([
         init/3,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type event_value() ::
        {'id', binary()} |
        {'event', binary()} |
        {'data', binary()} |
        {'retry', binary()}.

-type event() :: [event_value(), ...].

-type result() ::
    {'send', Event :: event(), NewState :: any()} |
    {'nosend', NewState :: any()} |
    {'stop', NewState :: any()}.

-callback init(InitArgs :: any(), LastEvtId :: any(), Req :: cowboy_req:req()) ->
    {ok, NewReq :: cowboy_req:req(), State :: any()} |
    {no_content, NewReq :: cowboy_req:req()} |
    {
      shutdown,
      StatusCode :: cowboy:http_status(),
      Headers :: cowboy:http_headers(),
      Body :: iodata(),
      NewReq :: cowboy_req:req()
    }.

-callback handle_notify(Msg :: any(), State :: any()) ->
    result().

-callback handle_info(Msg :: any(), State :: any()) ->
    result().

-callback handle_error(Msg :: any(), Reason :: any(), State :: any()) ->
    any().

-callback terminate(Reason :: any(),
                    Req :: cowboy_req:req(),
                    State :: any()) ->
    any().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type lasse_handler_option()  :: {'module', module()} | {'init_args', any()}.
-type lasse_handler_options() :: [module()] | [lasse_handler_option(), ...].

-spec init(any(), any(), lasse_handler_options()) -> {loop, any(), record(state)}.
init(_Transport, Req, Opts) ->
    Module = case get_value(module, Opts, Opts) of
                 Name when is_atom(Name) -> Name;
                 [Name] when is_atom(Name) -> Name;
                 _ -> throw(module_option_missing)
             end,
    InitArgs = get_value(init_args, Opts, []),
    {LastEventId, Req} = cowboy_req:header(<<"last-event-id">>, Req),
    InitResult = Module:init(InitArgs, LastEventId, Req),
    handle_init(InitResult, Module).

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

terminate(Reason, Req, State = #state{}) ->
    Module = State#state.module,
    ModuleState = State#state.state,
    Module:terminate(Reason, Req, ModuleState),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        {<<"GET">>, Req1} ->
            % "no-cache recommended to prevent caching of event data.
            Headers = [{<<"content-type">>, <<"text/event-stream">>},
                       {<<"cache-control">>, <<"no-cache">>}],
            {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req1),

            lists:foreach(
              fun(Event) -> ok = send_event(Event, Req2) end,
              InitialEvents
             ),

            {loop, Req2, #state{module = Module, state = State}};
        {_OtherMethod, _} ->
            Headers = [{<<"content-type">>, <<"text/html">>}],
            StatusCode = 405, % Method not Allowed
            cowboy_req:reply(StatusCode, Headers, Req),
            {shutdown, Req, #state{module = Module}}
    end;
handle_init({no_content, NewReq, State}, Module) ->
    cowboy_req:reply(204, [], NewReq),

    {shutdown, NewReq, #state{module = Module, state = State}};
handle_init({shutdown, StatusCode, Headers, Body, NewReq, State}, Module) ->
    cowboy_req:reply(StatusCode, Headers, Body, NewReq),

    {shutdown, NewReq, #state{module = Module, state = State}}.

process_result({send, Event, NewState}, Req, State) ->
    case send_event(Event, Req) of
        {error, Reason} ->
            Module = State#state.module,
            ModuleState = State#state.state,
            ErrorNewState = Module:handle_error(Event, Reason, ModuleState),
            {ok, Req, State#state{state = ErrorNewState}};
        ok ->
            {loop, Req, State#state{state = NewState}}
    end;
process_result({nosend, NewState}, Req, State) ->
    {loop, Req, State#state{state = NewState}};
process_result({stop, NewState}, Req, State) ->
    {ok, Req, State#state{state = NewState}}.

get_value(Key, PropList) ->
    case lists:keyfind(Key, 1, PropList) of
        {Key, Value} -> Value;
        _ -> undefined
    end.

get_value(Key, PropList, NotFound) ->
    case get_value(Key, PropList) of
        undefined -> NotFound;
        Value -> Value
    end.

send_event(Event, Req) ->
    EventMsg = build_event(Event),
    cowboy_req:chunk(EventMsg, Req).

build_event(Event) ->
    [build_comments(Event),
     build_field(<<"id: ">>, get_value(id, Event)),
     build_field(<<"event: ">>, get_value(name, Event)),
     build_data(get_value(data, Event)),
     build_field(<<"retry: ">>, get_value(retry, Event)),
     <<"\n">>].

build_comments(Event) ->
    Keys = [id, data, name, retry],
    Comments = lists:foldl(fun proplists:delete/2, Event, Keys),
    [build_comment(Val) || {_, Val} <- Comments].

build_comment(Comment) ->
    [[<<": ">>, X, <<"\n">>] || X <- binary:split(Comment, <<"\n">>, [global])].

build_field(_, undefined) ->
    [];
build_field(_, "") ->
    [];
build_field(Name, Value) ->
    [Name, Value, <<"\n">>].

build_data(undefined) ->
    throw(data_required);
build_data(Data) ->
    [[<<"data: ">>, X, <<"\n">>] || X <- binary:split(Data, <<"\n">>, [global])].
