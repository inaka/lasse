%%% @doc HTTP client for testing using gun.
-module(lasse_client).

-export([
         open/2,
         close/1,
         get/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application behavior functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec open(Host :: string(), Port :: integer()) -> {ok, pid()}.
open(Host, Port) ->
    proc_lib:spawn_link(fun () -> init(Host, Port) end).

-spec close(pid()) -> ok.
close(Pid) ->
    Pid ! 'shutdown',
    ok.

-spec get(Pid :: pid(), Url :: string()) -> ok.
get(Pid, Url) ->
    Pid ! {get, self(), Url},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Host, Port) ->
    Opts = [
            {type, tcp},
            {retry, 1},
            {retry_timeout, 1}
           ],
    {ok, Pid} = gun:open(Host, Port, Opts),
    loop(Pid).

loop(Pid) ->
    receive
        {get, From, Url} ->
            lager:info("Getting ~p", [Url]),
            StreamRef = gun:get(Pid, Url),
            From ! response(Pid, StreamRef),
            loop(Pid);
        shutdown ->
            gun:shutdown(Pid)
    end.

response(Pid, StreamRef) ->
    receive
        {'DOWN', _, _, _, Reason} ->
            exit(Reason);
        {gun_response, Pid, StreamRef, fin, _Status, _Headers} ->
            no_data;
        {gun_response, Pid, StreamRef, nofin, _Status, _Headers} ->
            receive_data(StreamRef);
        Msg ->
            lager:info("~p~n", [Msg]),
            exit({unexpected, Msg})
    after 5000 ->
            exit(response_timeout)
    end.

receive_data(StreamRef) ->
    receive_data(StreamRef, <<"">>).

receive_data(StreamRef, DataAcc) ->
    receive
        {'DOWN', _Tag, _, _, _Reason} ->
            {error, incomplete};
        {gun_data, _Pid, StreamRef, nofin, Data} ->
            receive_data(StreamRef, <<DataAcc/binary, Data/binary>>);
        {gun_data, _Pid, StreamRef, fin, Data} ->
            NewDataAcc = <<DataAcc/binary, Data/binary>>,
            lager:info("Received: ~p~n", [NewDataAcc]),
            NewDataAcc
    after 1000 ->
        {error, timeout}
    end.
