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

-callback init(InitArgs :: any(), Req :: cowboy_req:req()) ->
    {ok, NewReq :: cowboy_req:req(), State :: any()} |
    {
      shutdown, 
      StatusCode :: cowboy:http_status(), 
      Headers :: cowboy:http_headers(),
      Body :: iodata(),
      NewReq :: cowboy_req:req()
    }.

-callback handle_notify(Msg :: any(), State :: any()) ->
    {'send', Event :: event(), NewState :: any()} |
    {'nosend', NewState :: any()} |
    {'stop', NewState :: any()}.

-callback handle_info(Msg :: any(), State :: any()) ->
    {'send', Event :: event(), NewState :: any()} |
    {'nosend', NewState :: any()} |
    {'stop', NewState :: any()}.

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

    case Module:init(InitArgs, Req) of
        {ok, NewReq, State} ->
            Headers = [{<<"content-type">>, <<"text/event-stream">>},
                       % recommended to prevent caching of event data.
                       {<<"cache-control">>, <<"no-cache">>}],
            {ok, Req2} = cowboy_req:chunked_reply(200, Headers, NewReq),
            
            {loop, Req2, #state{module = Module, state = State}};
        {shutdown, StatusCode, Headers, Body, NewReq} ->
            cowboy_req:reply(StatusCode, Headers, Body, NewReq),
            
            {shutdown, NewReq, #state{}}
    end.

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

terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_result({send, Event, NewState}, Req, State) ->
    EventMsg = build_event(Event),
    ok = cowboy_req:chunk(EventMsg, Req),
    {loop, Req, State#state{state = NewState}};
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
