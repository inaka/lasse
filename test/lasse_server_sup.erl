-module(lasse_server_sup).
-behavior(supervisor).

-export([
         start_link/0,
         start_listeners/0
        ]).

-export([init/1]).

-spec start_link() -> {'ok', pid()} | {'error', any()}.
start_link() ->
  supervisor:start_link(?MODULE, {}).

-spec start_listeners() -> {ok, pid()} | {error, any()}.
start_listeners() ->
  Port = 8383,
  ListenerCount = 1,

  Dispatch =
    cowboy_router:compile(
      [ {'_',
         [
          {<<"/">>, cowboy_static, {file, "test/index.html"}},
          {<<"/events">>, lasse_handler, [events_handler]},
          {<<"/initial-events">>, lasse_handler, [initial_events_handler]},
          {<<"/shutdown">>, lasse_handler, [shutdown_handler]},
          {<<"/no_content">>, lasse_handler, [no_content_handler]}
         ]
        }
      ]),

  RanchOptions =
    [
      {port, Port}
    ],
  CowboyOptions =
    [
     {env,       [{dispatch, Dispatch}]},
     {compress,  true},
     {timeout,   12000}
    ],

  cowboy:start_http(
    http_lasse_server, ListenerCount, RanchOptions, CowboyOptions).

%%% Supervisor behavior functions

-spec init({}) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init({}) ->
  {
    ok,
    {
      {one_for_one, 5, 10},
      [
       {http_lasse_server,
          {lasse_server_sup, start_listeners, []},
          permanent, 1000, worker, [lasse_server_sup]}
      ]
    }}.
