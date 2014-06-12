%%% @doc Test suite for the Cowboy's Server-Sent Events handler.
-module(lasse_handler_SUITE).

-type config() :: [{atom(), term()}].

-export([all/0]).

-export([
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         send_and_receive_two_chunks/1,
         shutdown_check_response/1,
         init_without_module_option/1,
         init_with_module_option/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common test functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec all() -> [atom()].
all() ->
    [
     send_and_receive_two_chunks,
     shutdown_check_response,
     init_without_module_option,
     init_with_module_option
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    {ok, _Started} = application:ensure_all_started(lasse_server),

    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(lasse_server),

    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send_and_receive_two_chunks(config()) -> ok.
send_and_receive_two_chunks(_Config) ->
    %  Client connection is opened here
    % since doing it in init_per_suite and
    % providing the resulting Pid doesn't work.'
    Pid = open_conn(),
    % Provide the name to use when registering the process.
    ProcName = events_handler,
    Headers = [{<<"process-name">>, term_to_binary(ProcName)}],
    ok = lasse_client:get(Pid, "/events", Headers),

    Fun = fun() -> whereis(ProcName) =/= undefined end,
    wait_for(Fun, 100),

    % first chunk
    lasse_handler:notify(ProcName, <<"notify chunk">>),
    {chunk, <<"data: notify chunk\n\n">>} = response(),
    % second chunk
    events_handler ! <<"info chunk">>,
    {chunk, <<"data: info chunk\n\n">>} = response(),

    lasse_client:close(Pid).

-spec shutdown_check_response(config()) -> ok.
shutdown_check_response(_Config) ->
    Pid = open_conn(),
    ok = lasse_client:get(Pid, "/shutdown"),
    <<"Sorry, shutdown!">> = response(),

    lasse_client:close(Pid).

-spec init_without_module_option(config()) -> ok.
init_without_module_option(_Config) ->
    try
        Opts = [],
        lasse_handler:init({}, {}, Opts)
    catch
        throw:_ -> ok
    end,
    try
        Opts2 = [{init_args, []}],
        lasse_handler:init({}, {}, Opts2)
    catch
        throw:module_option_missing ->
            % lager:info("~p", [Error]),
            ok
    end.

-spec init_with_module_option(config()) -> ok.
init_with_module_option(_Config) ->
    try
        Opts = [{module, dummy_handler}],
        lasse_handler:init({}, {}, Opts)
    catch
        error:function_clause -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Waits for messages that are supposed to be a reponse
-spec response() -> binary() | {chunk, binary()}.
response() ->
    receive
        {response, Data} -> Data;
        {chunk, Chunk} -> {chunk, Chunk}
    after 5000 ->
            exit(no_event_from_server)
    end.

-spec open_conn() -> pid().
open_conn() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    Host = "localhost",
    lasse_client:open(Host, Port).

wait_for(Fun, Timeout) ->
    SleepTime = 10,
    Retries = erlang:round(Timeout / SleepTime),
    wait_for(Fun, SleepTime, Retries).

wait_for(_Fun, _SleepTime, 0) ->
    throw(timeout_while_waiting);
wait_for(Fun, SleepTime, Retries) ->
    case Fun() of
        true -> ok;
        _ ->
            timer:sleep(SleepTime),
            wait_for(Fun, SleepTime, Retries - 1)
    end.
