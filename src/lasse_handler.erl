-module(lasse_handler).

-export([
         init/3,
         info/3,
         terminate/3
        ]).

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

    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
    {loop, Req2, [{module, Module}]}.

info({message, Msg}, Req, State = [{module, Module}]) ->
    case Module:handle(Msg) of
        {ok, {Id, Data}} ->
            Message = ["id: ", Id, build_data(Data), "\n\n"],
            ok = cowboy_req:chunk(Message, Req),
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

build_data(Data) ->
    build_data(Data, []).

build_data([], BinaryData) ->
    lists:reverse(BinaryData);
build_data([Data|MoreData], BinaryData) ->
    build_data(MoreData, [["\ndata:", Data] | BinaryData]);
build_data(Data, BinaryData) ->
    build_data([Data], BinaryData).
