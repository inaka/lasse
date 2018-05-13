%%% @doc Test suite for the Cowboy's Server-Sent Events handler.
-module(lasse_handler_SUITE).

-dialyzer([ {nowarn_function, [ init_without_module_option/1
                              , init_with_module_option/1
                              , cause_chunk_to_fail/1
                              ]
            }]).

-type config() :: [{atom(), term()}].

-record(state, { module :: module()
               , state :: any()
               }).

-type state() :: #state{}.

-export([all/0]).

-export([ init_per_suite/1
        , end_per_suite/1
        ]).

-export([ send_and_receive_two_chunks/1
        , send_and_do_not_receive_anything/1
        , send_and_receive_initial_events/1
        , send_data_and_id/1
        , send_comments_and_data/1
        , do_not_send_data/1
        , send_post_and_fail/1
        , check_no_content/1
        , send_last_event_id/1
        , cause_chunk_to_fail/1
        , shutdown/1
        , init_without_module_option/1
        , init_with_module_option/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common test functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group],
  [F || {F, 1} <- module_info(exports), not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, [_|_]} = application:ensure_all_started(lasse_server),
  {ok, [_|_]} = application:ensure_all_started(shotgun),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(lasse_server),
  ok = application:stop(shotgun),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send_and_receive_two_chunks(config()) -> ok.
send_and_receive_two_chunks(_Config) ->
  %  Client connection is opened here
  % since doing it in init_per_suite and
  % providing the resulting Pid doesn't work.'
  Pid = open_conn(),
  ProcName = send_and_receive_two_chunks,
  get(Pid, ProcName, "/events"),

  % first chunk
  lasse_handler:notify(ProcName, send),
  [#{data := <<"notify chunk\n">>}] = get_events(Pid),

  % second chunk
  ProcName ! send,
  [#{data := <<"info chunk\n">>}] = get_events(Pid),

  lasse_handler:notify(ProcName, stop),
  close_conn(Pid).

-spec send_and_do_not_receive_anything(config()) -> ok.
send_and_do_not_receive_anything(_Config) ->
  Pid = open_conn(),
  ProcName = send_and_do_not_receive_anything,
  get(Pid, ProcName, "/events"),

  % first chunk
  lasse_handler:notify(ProcName, nosend),
  ok = get_no_events(Pid),

  % second chunk
  ProcName ! nosend,
  ok = get_no_events(Pid),

  lasse_handler:notify(ProcName, stop),
  close_conn(Pid).

-spec send_and_receive_initial_events(config()) -> ok.
send_and_receive_initial_events(_Config) ->
  Pid = open_conn(),
  get(Pid, undefined, "/initial-events"),

  [ #{data := <<"initial 1\n">>}
  , #{data := <<"initial 2\n">>}
  ] = get_events(Pid),

  close_conn(Pid).

-spec send_data_and_id(config()) -> ok.
send_data_and_id(_Config) ->
  Pid = open_conn(),
  ProcName = send_data_and_id,
  get(Pid, ProcName, "/events"),

  lasse_handler:notify(ProcName, send_id),
  [#{id := <<"1">>, data := <<"notify chunk\n">>}] = get_events(Pid),

  lasse_handler:notify(ProcName, stop),
  close_conn(Pid).

-spec send_comments_and_data(config()) -> ok.
send_comments_and_data(_Config) ->
  Pid = open_conn(),
  ProcName = send_comments_and_data,
  get(Pid, ProcName, "/events"),

  lasse_handler:notify(ProcName, comments),
  Chunk = <<
            ": Comment 1\n",
            ": Comment 2\n",
            "data: some data"
          >>,
  [{_, _, Chunk}] = get_raw_events(Pid),

  lasse_handler:notify(ProcName, stop),
  close_conn(Pid).

-spec do_not_send_data(config()) -> ok.
do_not_send_data(_Config) ->
  Pid = open_conn(),
  ProcName = do_not_send_data,
  get(Pid, ProcName, "/events"),
  lasse_handler:notify(ProcName, no_data),
  ok = get_no_events(Pid),
  close_conn(Pid).

-spec send_post_and_fail(config()) -> ok.
send_post_and_fail(_Config) ->
  Pid = open_conn(),
  ProcName = send_post_and_fail,

  #{status_code := 405} = post(Pid, ProcName, "/events"),

  close_conn(Pid).

-spec check_no_content(config()) -> ok.
check_no_content(_Config) ->
  Pid = open_conn(),

  #{status_code := 204} = raw_get(Pid, "/no_content"),

  close_conn(Pid).

-spec send_last_event_id(config()) -> ok.
send_last_event_id(_Config) ->
  Pid = open_conn(),
  ProcName = send_last_event_id,

  get(Pid, ProcName, "/events", #{<<"last-event-id">> => <<"42">>}),

  lasse_handler:notify(ProcName, last_event_id),
  [#{data := <<"42\n">>}] = get_events(Pid),

  close_conn(Pid).

-spec cause_chunk_to_fail(config()) -> {ok, cowboy_req:req(), state()}.
cause_chunk_to_fail(_Config) ->
  ok = try
    Req = {},
    State = #state{module = events_handler , state = {}},
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, stream_body, fun(_, _, _) -> error end),
    {ok, Req, _}  = lasse_handler:info(send, Req, State)
  catch
    error:{try_clause, error} -> ok
  after
    meck:unload(cowboy_req)
  end.

-spec shutdown(config()) -> ok.
shutdown(_Config) ->
  Pid = open_conn(),

  #{ status_code := 404
   , body := <<"Sorry, shutdown!">>
   } = raw_get(Pid, "/shutdown"),

  {ok, _Ref} =
    shotgun:get(Pid, "/shutdown", #{}, #{async => true, async_mode => sse}),

  get_no_events(Pid),

  close_conn(Pid).

-spec init_without_module_option(config()) -> ok.
init_without_module_option(_Config) ->
  ok = try
         Opts = [],
         lasse_handler:init({}, Opts),
         fail
       catch
         throw:module_option_missing -> ok
       end,
  ok = try
         Opts2 = #{init_args => []},
         lasse_handler:init({}, Opts2),
         fail
       catch
         throw:module_option_missing -> ok
       end.

-spec init_with_module_option(config()) -> {loop, cowboy_req:req(), state()}.
init_with_module_option(_Config) ->
  try
    Request = {},
    Module = dummy_handler,
    State = #state{module = Module, state = {}},

    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, method, fun(_Req) -> <<"GET">> end),
    StreamReply = fun(_, _, Req) ->  Req end,
    meck:expect(cowboy_req, stream_reply, StreamReply),
    meck:expect(cowboy_req, header, fun(_, _) -> undefined end),
    Opts = #{module => Module},
    {cowboy_loop, Request, State} = lasse_handler:init({}, Opts)
  after
    catch meck:unload(cowboy_req)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_conn() ->
  {ok, Pid} = shotgun:open("localhost", 8383),
  Pid.

close_conn(Pid) -> shotgun:close(Pid).

get(Pid, Name, Url) -> get(Pid, Name, Url, #{}).

-spec get(pid(), atom(), string(), map()) -> ok.
get(Pid, Name, Uri, Headers) ->
  NewHeaders = Headers#{<<"process-name">> => atom_to_binary(Name, utf8)},
  {ok, _Ref} =
      shotgun:get(Pid, Uri, NewHeaders, #{async => true, async_mode => sse}),

  case Name of
    undefined -> ok;
    Name -> true = ktn_task:wait_for(fun() -> whereis(Name) =/= undefined end, true)
  end.

raw_get(Pid, Uri) ->
  {ok, Response} = shotgun:get(Pid, Uri),
  Response.

post(Pid, Name, Url) ->
  Headers = #{<<"process-name">> => atom_to_binary(Name, utf8)},
  {ok, Response} = shotgun:post(Pid, Url, Headers, [], #{}),
  Response.

get_events(Pid) ->
  ktn_task:wait_for_success(
    fun() ->
      try
        ct:log("waiting for events at ~p", [self()]),
        timer:sleep(100),
        Events = shotgun:events(Pid),
        ct:log("Events: ~p", [Events]),
        [_|_] = [shotgun:parse_event(Bin) || {_, _, Bin} <- Events]
      catch
        _:Error ->
          ct:log("Failed: ~p", [Error]),
          throw(Error)
      end
    end).

get_raw_events(Pid) ->
  ktn_task:wait_for_success(
    fun() ->
      ct:log("waiting for events at ~p", [self()]),
      Events = shotgun:events(Pid),
      ct:log("Events: ~p", [Events]),
      [_|_] = Events
    end).

get_no_events(Pid) ->
  {error, {timeout, {badmatch, []}}} = get_events(Pid),
  ok.
