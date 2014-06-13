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
         send_and_do_not_receive_anything/1,
         send_data_and_id/1,
         do_not_send_data/1,
         shutdown_check_response/1,
         init_without_module_option/1,
         init_with_module_option/1
        ]).

-define(current_function(),
        element(2, element(2, process_info(self(), current_function)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common test functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec all() -> [atom()].
all() ->
    [
     send_and_receive_two_chunks,
     send_and_do_not_receive_anything,
     send_data_and_id,
     do_not_send_data,
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

send_and_receive_two_chunks(_Config) ->
    %  Client connection is opened here
    % since doing it in init_per_suite and
    % providing the resulting Pid doesn't work.'
    Pid = open_conn(),
    ProcName = ?current_function(),
    get(Pid, ProcName, "/events"),

    % first chunk
    lasse_handler:notify(ProcName, send),
    check_response(Pid, {chunk, <<"data: notify chunk\n\n">>}),

    % second chunk
    ProcName ! send,
    check_response(Pid, {chunk, <<"data: info chunk\n\n">>}),

    lasse_handler:notify(ProcName, stop),
    lasse_client:close(Pid).

send_and_do_not_receive_anything(_Config) ->
    Pid = open_conn(),
    ProcName = ?current_function(),
    get(Pid, ProcName, "/events"),

    % first chunk
    lasse_handler:notify(ProcName, nosend),

    ok = try
             check_response(Pid, {chunk, <<"data: notify chunk\n\n">>}),
             fail
         catch
             error:timeout_while_waiting -> ok
         end,

    % second chunk
    ProcName ! nosend,
    ok = try
             check_response(Pid, {chunk, <<"data: info chunk\n\n">>}),
             fail
         catch
             error:timeout_while_waiting -> ok
         end,

    lasse_handler:notify(ProcName, stop),
    lasse_client:close(Pid).

send_data_and_id(_Config) ->
    Pid = open_conn(),
    ProcName = ?current_function(),
    get(Pid, ProcName, "/events"),

    lasse_handler:notify(ProcName, send_id),

    check_response(Pid, {chunk, <<"id: 1\ndata: notify chunk\n\n">>}),

    lasse_handler:notify(ProcName, stop),
    lasse_client:close(Pid).

do_not_send_data(_Config) ->
    Pid = open_conn(),
    ProcName = ?current_function(),
    get(Pid, ProcName, "/events"),

    lasse_handler:notify(ProcName, no_data),
    ok = try
             check_response(Pid, {chunk, <<"id: 1\ndata: notify chunk\n\n">>}),
             fail
         catch
             error:timeout_while_waiting -> ok
         end,

    lasse_client:close(Pid).

shutdown_check_response(_Config) ->
    Pid = open_conn(),

    ok = lasse_client:start_get(Pid, "/shutdown"),
    check_response(Pid, {response, <<"Sorry, shutdown!">>}),

    lasse_client:close(Pid).

init_without_module_option(_Config) ->
    ok = try
             Opts = [],
             lasse_handler:init({}, {}, Opts),
             fail
         catch
             throw:_ -> ok
         end,
    ok = try
             Opts2 = [{init_args, []}],
             lasse_handler:init({}, {}, Opts2),
             fail
         catch
             throw:module_option_missing -> ok
         end.

init_with_module_option(_Config) ->
    ok = try
             Opts = [{module, dummy_handler}],
             lasse_handler:init({}, {}, Opts),
             fail
         catch
             error:function_clause -> ok
         end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec open_conn() -> pid().
open_conn() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    Host = "localhost",
    {ok, Pid} = lasse_client:connect(Host, Port),
    Pid.

-spec get(Pid :: pid(), Name :: atom(), Url :: string()) -> ok.
get(Pid, Name, Url) ->
    Headers = [{<<"process-name">>, term_to_binary(Name)}],
    ok = lasse_client:start_get(Pid, Url, Headers),

    Fun = fun() -> whereis(Name) =/= undefined end,
    wait_for(Fun, 100).

wait_for(Fun, Timeout) ->
    SleepTime = 10,
    Retries = Timeout div SleepTime,
    wait_for(Fun, SleepTime, Retries).

wait_for(_Fun, _SleepTime, 0) ->
    error(timeout_while_waiting);
wait_for(Fun, SleepTime, Retries) ->
    case Fun() of
        true -> ok;
        _ ->
            timer:sleep(SleepTime),
            wait_for(Fun, SleepTime, Retries - 1)
    end.

check_response(Pid, Response) ->
    Fun = fun() -> Response =:= lasse_client:pop(Pid) end,
    wait_for(Fun, 100).
