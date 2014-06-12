%%% @doc HTTP client for testing using gun.
-module(lasse_client).

-export([
         open/2,
         close/1,
         get/2,
         get_chunked/2
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

-spec get_chunked(Pid :: pid(), Url :: string()) -> ok.
get_chunked(Pid, Url) ->
    Pid ! {get_chunked, self(), Url},
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

%% @doc loop that holds a gun connection and accepts.
loop(Pid) ->
    receive
        {get, From, Url} ->
            lager:info("Getting ~p", [Url]),
            StreamRef = gun:get(Pid, Url),
            response(Pid, StreamRef, From),
            loop(Pid);
        {get_chunked, From, Url} ->
            lager:info("Getting chunked ~p", [Url]),
            StreamRef = gun:get(Pid, Url),
            response(Pid, StreamRef, From, true);
        shutdown ->
            gun:shutdown(Pid)
    end.

response(Pid, StreamRef, From) ->
    response(Pid, StreamRef, From, false).

response(Pid, StreamRef, From, Chunked) ->
    receive
        {'DOWN', _, _, _, Reason} ->
            exit(Reason);
        {gun_response, Pid, StreamRef, fin, _Status, _Headers} ->
            no_data;
        {gun_response, Pid, StreamRef, nofin, _Status, _Headers} ->
            receive_response(StreamRef, From, Chunked)
            % From ! {response, receive_data(StreamRef)}
    after 5000 ->
            exit(response_timeout)
    end.

receive_response(StreamRef, From, false) ->
    receive_data(StreamRef, From);
receive_response(StreamRef, From, true) ->
    receive_chunks(StreamRef, From).

receive_data(StreamRef, From) ->
    receive_data(StreamRef, From, <<"">>).

receive_data(StreamRef, From, DataAcc) ->
    receive
        {'DOWN', _Tag, _, _, _Reason} ->
            {error, incomplete};
        {gun_data, _Pid, StreamRef, nofin, Data} ->
            receive_data(StreamRef, <<DataAcc/binary, Data/binary>>);
        {gun_data, _Pid, StreamRef, fin, Data} ->
            From ! {response, <<DataAcc/binary, Data/binary>>}
    after 5000 ->
        {error, timeout}
    end.

receive_chunks(StreamRef, From) ->
    receive
        {'DOWN', _Tag, _, _, _Reason} ->
            {error, incomplete};
        {gun_data, _Pid, StreamRef, nofin, Data} ->
            lager:info("Chunk: ~p", [Data]),
            From ! {chunk, Data},
            receive_chunks(StreamRef, From);
        {gun_data, _Pid, StreamRef, fin, Data} ->
            lager:info("Chunk: ~p", [Data]),
            From ! {chunk, Data},
            ok
    after 5000 ->
        {error, timeout}
    end.
