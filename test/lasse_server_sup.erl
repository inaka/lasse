-module(lasse_server_sup).
-behavior(supervisor).

-export([
         start_link/0,
         start_listeners/0
        ]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, {}).

start_listeners() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    {ok, ListenerCount} = application:get_env(cowboy, http_listener_count),

    Dispatch =
        cowboy_router:compile(
          [ {'_',
             [
              {<<"/">>, cowboy_static, {file, "test/index.html"}},
              {<<"/events">>, lasse_handler, [events_handler]},
              {<<"/shutdown">>, lasse_handler, [shutdown_handler]}
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

    cowboy:start_http(http_lasse_server, ListenerCount, RanchOptions, CowboyOptions).

%%% Supervisor behavior functions

init({}) ->
    {
      ok,
      {
        {one_for_one, 5, 10},
        [
         {http_lasse_server, {lasse_server_sup, start_listeners, []}, permanent, 1000, worker, [lasse_server_sup]}
        ]
      }}.
