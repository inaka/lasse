% @doc Server-Side Event handler for Cowboy 
-module(lasse_handler).

-export([
         init/3,
         info/3,
         terminate/3
        ]).

-record(event, {id, name, data, retry}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type lasse_sse_message() :: tuple(Event :: string(), Data :: binary() | [binary()]).
-type lasse_sse_error() :: tuple('error', Reason :: string()).

-callback init() -> 'ok' | lasse_sse_error().
-callback handle(Msg :: any()) -> tuple('ok', 'terminate' | lasse_sse_message()) | lasse_sse_error().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type lasse_handler_option()  :: {'module', module()}.
-type lasse_handler_options() :: [module(), ...] | [lasse_handler_option(), ...].
-type lasse_handler_state() :: [{'module', module()}].

-spec init(any(), any(), lasse_handler_options()) -> {loop, any(), lasse_handler_state()}.
init(_Transport, Req, Opts) ->
    Module = module_option(Opts),
    Module:init(), % Should this accept an argument?

    Headers = [{<<"content-type">>, <<"text/event-stream">>},
               {<<"cache-control">>, <<"no-cache">>}], % recommended to prevent caching of event data.
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
    {loop, Req2, [{module, Module}]}.

info({message, Msg}, Req, State = [{module, Module}]) ->
    case Module:handle(Msg) of
        {ok, Event} when is_record(Event, event) ->
            EventMsg = build_event(Event),
            ok = cowboy_req:chunk(EventMsg, Req),
            {loop, Req, State};            
        {ok, terminate} ->
            {ok, Req, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec module_option(lasse_handler_options()) -> module().
module_option(Opts) ->
    case lists:keyfind(module, 1, Opts) of
        {module, Module} -> Module;
        _                   -> [Module] = Opts,
                               Module
    end.


build_event(#event{id=Id, name=Name, data=Data, retry=Retry}) ->
    [build_field("id: ", Id),
     build_field("event: ", Name),
     build_data(Data),
     build_field("retry: ", Retry)].


build_field(_, undefined) ->
    [];
build_field(_, "") ->
    [];
build_field(Name, Value) ->
    [Name, Value].    


build_data(Data) ->
    build_data(Data, []).

build_data([], Result) ->
    lists:reverse(Result);
build_data([Data|MoreData], Result) ->
    build_data(MoreData, [["\ndata:", Data] | Result]);
build_data(Data, Result) when is_binary(Data) ->
    SplitData = binary:split(Data, <<"\n">>, [global]),
    build_data(SplitData, Result).
