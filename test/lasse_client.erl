%%% @doc HTTP client for testing using gun.
-module(lasse_client).
-behavior(gen_fsm).

%%% Public API
-export([
         connect/2,
         close/1,
         start_get/2,
         start_get/3,
         start_post/2,
         start_post/3,
         pop/1
        ]).

%%% gen_fsm callbacks
-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

%%% State functions
-export([
         open/2,
         wait_response/2,
         receive_data/2,
         receive_chunk/2
        ]).

-type response() :: {response, binary()} | {chunk, binary()}.

-record (state,
         {
           pid :: pid(),
           stream :: reference(),
           data = <<"">> :: binary(),
           responses = queue:new() :: queue:queue(response())
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec connect(Host :: string(), Port :: integer()) -> {ok, pid()}.
connect(Host, Port) ->
    gen_fsm:start(lasse_client, [Host, Port], []).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_fsm:send_all_state_event(Pid, 'shutdown'),
    ok.

-spec start_get(Pid :: pid(), Url :: string()) -> ok.
start_get(Pid, Url) ->
    start_get(Pid, Url, []),
    ok.

-type headers() :: [{binary(), binary()}].

-spec start_get(Pid :: pid(), Url :: string(), Headers :: headers()) -> ok.
start_get(Pid, Url, Headers) ->
    gen_fsm:send_event(Pid, {get, Url, Headers}).

-spec start_post(Pid :: pid(), Url :: string()) -> ok.
start_post(Pid, Url) ->
    start_post(Pid, Url, []),
    ok.

-spec start_post(Pid :: pid(), Url :: string(), Headers :: headers()) -> ok.
start_post(Pid, Url, Headers) ->
    gen_fsm:send_event(Pid, {post, Url, Headers}).

-spec pop(Pid :: pid()) -> {response, binary()} | {chunk, binary()}.
pop(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Host, Port]) ->
    Opts = [
            {type, tcp},
            {retry, 1},
            {retry_timeout, 1}
           ],
    {ok, Pid} = gun:open(Host, Port, Opts),
    lager:info("gun pid: ~p - fsm pid: ~p", [Pid, self()]),
    {ok, open, #state{pid = Pid}}.

handle_event(shutdown, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_sync_event(get_response, _From, StateName, State) ->
    {Reply, Responses} = case queue:out(State#state.responses) of
                             {{value, Response}, NewQueue} ->
                                 {Response, NewQueue};
                             {empty, Queue} ->
                                 {no_data, Queue}
                         end,
    NewState = State#state{responses = Responses},
    {reply, Reply, StateName, NewState}.

handle_info(Event, StateName, StateData) ->
    lager:info("Forwarding info event to state ~p", [StateName]),
    ?MODULE:StateName(Event, StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, #state{pid = Pid}) ->
    gun:shutdown(Pid),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open({get, Url, Headers}, State = #state{pid = Pid}) ->
    StreamRef = gun:get(Pid, Url, Headers),
    lager:info("Getting ~p, ref ~p", [Url, StreamRef]),

    {next_state, wait_response, State#state{stream = StreamRef}};
open({post, Url, Headers}, State = #state{pid = Pid}) ->
    StreamRef = gun:post(Pid, Url, Headers),
    lager:info("Posting ~p, ref ~p", [Url, StreamRef]),

    {next_state, wait_response, State#state{stream = StreamRef}}.

wait_response({'DOWN', _, _, _, Reason}, _State) ->
    exit(Reason);
wait_response({gun_response, Pid, StreamRef, fin, StatusCode, _},
              State = #state{pid = Pid, stream = StreamRef}) ->
    lager:info("~p: ~p", [StatusCode, response_fin]),

    Responses = State#state.responses,
    NewResponses = queue:in({no_response, StatusCode}, Responses),

    NewState = State#state{responses = NewResponses},

    {next_state, done, NewState};
wait_response({gun_response, Pid, StreamRef, nofin, _, Headers},
              State = #state{pid = Pid, stream = StreamRef}) ->
    lager:info("~p", [response_nofin]),
    StateName = case lists:keyfind(<<"transfer-encoding">>, 1, Headers) of
                    {<<"transfer-encoding">>, <<"chunked">>} ->
                        receive_chunk;
                    _ ->
                        receive_data
                end,
    {next_state, StateName, State};
wait_response(Event, State) ->
    lager:info("~p", Event),
    lager:info("~p", State),
    {stop, unexpected, State}.


%% Regular response
receive_data({'DOWN', _, _, _, _Reason}, _State) ->
    error(incomplete);
receive_data({gun_data, Pid, StreamRef, nofin, Data},
             State = #state{pid = Pid, stream = StreamRef, data = DataAcc}) ->
    NewData = <<DataAcc/binary, Data/binary>>,
    NewState = State#state{data = NewData},

    {next_state, receive_data, NewState};
receive_data({gun_data, Pid, StreamRef, fin, Data},
             State = #state{pid = Pid, stream = StreamRef, data = DataAcc}) ->
    NewData = <<DataAcc/binary, Data/binary>>,

    Responses = State#state.responses,
    NewResponses = queue:in({response, NewData}, Responses),

    NewState = State#state{responses = NewResponses},

    {next_state, open, NewState};
receive_data({gun_error, Pid, StreamRef, Reason},
              State = #state{pid = Pid, stream = StreamRef}) ->
    lager:error("gun_error: ~p", [Reason]),
    {next_state, open, State}.

%% Chunked data response
receive_chunk({'DOWN', _, _, _, _Reason}, _State) ->
    error(incomplete);
receive_chunk({gun_data, Pid, StreamRef, nofin, Data},
               State = #state{pid = Pid, stream = StreamRef}) ->
    Responses = State#state.responses,
    NewResponses = queue:in({chunk, Data}, Responses),

    NewState = State#state{responses = NewResponses},

    {next_state, receive_chunk, NewState};
receive_chunk({gun_data, Pid, StreamRef, fin, Data},
               State = #state{pid = Pid, stream = StreamRef}) ->
    Responses = State#state.responses,
    NewResponses = queue:in({chunk, Data}, Responses),

    NewState = State#state{responses = NewResponses},

    {next_state, open, NewState};
receive_chunk({gun_error, Pid, StreamRef, Reason},
              State = #state{pid = Pid, stream = StreamRef}) ->
    lager:error("gun_error: ~p", [Reason]),
    {next_state, open, State}.
