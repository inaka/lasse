-module(lasse_server).
-behavior(application).

-export([
         start/2,
         stop/1
        ]).

%%% Behavior

%% @private
start(_Type, _Args) ->
    lasse_server_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_lasse_server).
