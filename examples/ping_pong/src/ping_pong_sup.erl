-module(ping_pong_sup).
-behavior(supervisor).

-export([
         start_link/0,
         start_listeners/0
        ]).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?MODULE, {}).

start_listeners() ->
    Port = 8181,
    ListenerCount = 1,

    Dispatch =
        cowboy_router:compile(
          [ {'_',
             [
              {<<"/">>, cowboy_static, {file, "src/index.html"}},
              {<<"/ping">>, ping_handler, []},
              {<<"/pong">>, lasse_handler, [pong_handler]}
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
      http_ping_pong_server,
      ListenerCount,
      RanchOptions,
      CowboyOptions
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Supervisor Behavior Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({}) ->
    ok = pg2:create(pongers),
    {
      ok,
      {
        {one_for_one, 5, 10},
        [
         {
           http_ping_pong_server,
           {ping_pong_sup, start_listeners, []},
           permanent,
           1000,
           worker,
           [ping_pong_sup]
         }
        ]
      }}.
