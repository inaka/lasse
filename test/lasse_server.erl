-module(lasse_server).
-behavior(application).

-export([
         start/2,
         stop/1
        ]).

%%% Behavior

%% @private
-spec start(atom(), term()) -> {'ok', pid()} | {'error', any()}.
start(_Type, _Args) ->
    lasse_server_sup:start_link().

-spec stop(term()) -> ok | {error, not_found}.
stop(_State) ->
    cowboy:stop_listener(http_lasse_server).
