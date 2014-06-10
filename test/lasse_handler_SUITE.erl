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
    {ok, Port} = application:get_env(cowboy, http_port),
    {ok, Pid} = lasse_client:start(Port),
    [{client, Pid} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(test_server),
    application:stop(test_client),
    Config.

%% @doc Try to do a post.
-spec not_allowed_method(config()) -> ok.
not_allowed_method(_Config) ->
    io:format("INIT!!!!!").
