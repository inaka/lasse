%%% @doc Server-Sent Event handler for Cowboy
-module(lasse_handler).

-export([
         init/3,
         info/3,
         notify/2,
         terminate/3
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
    {'send', Event :: event(), NewState :: record(state)} |
    {'nosend', NewState :: record(state)} |
    {'stop', NewState :: record(state)}.

-callback init(InitArgs :: any(), Req :: cowboy_req:req()) ->
    {ok, NewReq :: cowboy_req:req(), State :: record(state)} |
    {
      shutdown, 
      StatusCode :: cowboy:http_status(), 
      Headers :: cowboy:http_headers(),
      Body :: iodata(),
      NewReq :: cowboy_req:req()
    }.

-callback handle_notify(Msg :: any(), State :: record(state)) ->
    result().

-callback handle_info(Msg :: any(), State :: record(state)) ->
    result().

-callback handle_error(Msg :: any(), Reason :: any(), State :: record(state)) ->
    any().

-callback terminate(Reason :: any(),
                    Req :: cowboy_req:req(),
                    State :: record(state)) ->
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

    InitResult = Module:init(InitArgs, Req),
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

notify(Pid, Msg) ->
    Pid ! {message, Msg}.

terminate(Reason, Req, State) ->
    Module = State#state.module,
    ModuleState = State#state.state,
    lager:info("Terminating module: ~p", [Module]),
    Module:terminate(Reason, Req, ModuleState),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_init({ok, Req, State}, Module) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req1} ->
            % "no-cache recommended to prevent caching of event data.
            Headers = [{<<"content-type">>, <<"text/event-stream">>},
                       {<<"cache-control">>, <<"no-cache">>}],
            {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req1),

            {loop, Req2, #state{module = Module, state = State}};
        {_OtherMethod, _} ->
            Headers = [{<<"content-type">>, <<"text/html">>}],
            StatusCode = 405, % Method not Allowed
            cowboy_req:reply(StatusCode, Headers, Req),
            {shutdown, Req, #state{module = Module}}
    end;
handle_init({shutdown, StatusCode, Headers, Body, NewReq}, Module) ->
    cowboy_req:reply(StatusCode, Headers, Body, NewReq),

    {shutdown, NewReq, #state{module = Module}}.

process_result({send, Event, NewState}, Req, State) ->
    EventMsg = build_event(Event),
    case cowboy_req:chunk(EventMsg, Req) of
        {error, Reason} ->
            Module = State#state.module,
            ModuleState = State#state.state,
            NewModuleState = Module:handle_error(EventMsg, Reason, ModuleState),
            {ok, Req, State#state{module = NewModuleState}};
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

build_event(Event) ->
    [build_field(<<"id: ">>, get_value(id, Event)),
     build_field(<<"event: ">>, get_value(name, Event)),
     build_data(get_value(data, Event)),
     build_field(<<"retry: ">>, get_value(retry, Event)),
     <<"\n">>].

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
