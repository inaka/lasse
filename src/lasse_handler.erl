% @doc Server-Side Event handler for Cowboy 
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

-record(state, {module, state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type event_value() :: {'id', binary()} | {'event', binary()} | {'data', binary()} | {'retry', binary()}.
-type event() :: [event_value(), ...].

-callback init(InitArgs :: any()) -> State :: any().
-callback handle_notify(Msg :: any(), State :: any()) ->
    tuple('send', Event :: event(), NewState :: any())
        | tuple('nosend', NewState :: any())
        | tuple('stop', NewState :: any()).
-callback handle_info(Msg :: any(), State :: any()) ->
    tuple('send', Event :: event(), NewState :: any())
        | tuple('nosend', NewState :: any())
        | tuple('stop', NewState :: any()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type lasse_handler_option()  :: {'module', module()} | {'init_args', any()}.
-type lasse_handler_options() :: [module()] | [lasse_handler_option(), ...].

-spec init(any(), any(), lasse_handler_options()) -> {loop, any(), record(state)}.
init(_Transport, Req, Opts) ->
    Module = get_value(module, Opts, hd(Opts)),
    InitArgs = get_value(init_args, Opts, []),

    State = Module:init(InitArgs),

    Headers = [{<<"content-type">>, <<"text/event-stream">>},
               {<<"cache-control">>, <<"no-cache">>}], % recommended to prevent caching of event data.
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),

    {loop, Req2, #state{module = Module, state = State}}.

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

process_result(Result, Req, State) ->
    case Result of
        {send, Event, NewState}  ->
            EventMsg = build_event(Event),
            ok = cowboy_req:chunk(EventMsg, Req),
            {loop, Req, State#state{state = NewState}};
        {nosend, NewState} ->
            {loop, Req, State#state{state = NewState}};
        {stop, NewState} ->
            {ok, Req, State#state{state = NewState}}
    end.

get_value(Key, PropList) ->
    case lists:keyfind(Key, 1, PropList) of
        {Key, Value} -> Value;
        _ -> false
    end.

get_value(Key, PropList, NotFound) ->
    case get_value(Key, PropList) of
        false -> NotFound;
        Value -> Value
    end.

build_event(Event) ->
    [build_field(<<"id: ">>, get_value(id, Event)),
     build_field(<<"event: ">>, get_value(name, Event)),
     build_data(get_value(data, Event)),
     build_field(<<"retry: ">>, get_value(retry, Event)),
     <<"\n">>].

build_field(_, false) ->
    [];
build_field(_, "") ->
    [];
build_field(Name, Value) ->
    [Name, Value, <<"\n">>].

build_data(false) ->
    throw(data_required);
build_data(Data) ->
    build_data(Data, []).

build_data([], Result) ->
    lists:reverse(Result);
build_data([Data|MoreData], Result) ->
    build_data(MoreData, [[<<"data: ">>, Data, <<"\n">>] | Result]);
build_data(Data, Result) when is_binary(Data) ->
    SplitData = binary:split(Data, <<"\n">>, [global]),
    build_data(SplitData, Result).
