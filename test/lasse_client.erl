%%% @doc HTTP client for testing using gun.
-module(lasse_client).

-export([
         start/1,
         stop/1
        ]).

%%% Application behavior functions

%% @private
-spec start(Port :: integer()) -> {ok, pid()}.
start(Port) ->
    gun:open("localhost", Port).

stop(Pid) ->
    gun:shutdown(Pid),
    ok.
