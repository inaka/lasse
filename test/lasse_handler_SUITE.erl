%%% @doc Test suite for the Cowboy's Server-Sent Events handler.
-module(lasse_handler_SUITE).

-type config() :: [{atom(), term()}].

-export([all/0]).

-export([
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         not_allowed_method/1
        ]).

%% @private
-spec all() -> [atom()].
all() ->
    [not_allowed_method].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    {ok, _Started} = application:ensure_all_started(lasse_server),

    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(lasse_server),

    Config.

%% @doc Try to do a post.
-spec not_allowed_method(config()) -> ok.
not_allowed_method(Config) ->
    %  Client connection is opened here
    % since doing it in init_per_suite and
    % providing the resulting Pid doesn't work.'
    Pid = open_conn(),
    ok = lasse_client:get(Pid, "/events"),
    receive
         Data -> Data
    after 5000 ->
            exit(no_event_from_server)
    end,
    lasse_client:close(Pid),
    Config.

open_conn() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    Host = "localhost",
    Pid = lasse_client:open(Host, Port),
    %% Host = "www.google.com",
    %% Pid = lasse_client:open(Host, 80),
    Pid.
