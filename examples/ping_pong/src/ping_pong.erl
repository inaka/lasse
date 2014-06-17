-module(ping_pong).
-behavior(application).

-export([
         start/0,
         start/2,
         stop/1
        ]).

start() ->
    {ok, _ } = application:ensure_all_started(ping_pong).

%%% Behavior

%% @private
start(_Type, _Args) ->
    ping_pong_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_ping_pong_server),
    ok.
