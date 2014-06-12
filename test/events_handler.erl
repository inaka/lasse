-module(events_handler).
-behavior(lasse_handler).

-export([
         init/2,
         handle_info/2,
         handle_notify/2
        ]).

init(_InitArgs, Req) ->
    % Take process name from the "process-name" header.
    {Headers, _} = cowboy_req:headers(Req),
    {<<"process-name">>, ProcNameBin} = lists:keyfind(<<"process-name">>, 1, Headers),
    ProcName = binary_to_term(ProcNameBin),

    register(ProcName, self()),
    lager:info("Initiating an ~p in ~p", [ProcName, whereis(ProcName)]),

    {ok, Req, {}}.

handle_info(Msg, State) ->
    {send, [{data, Msg}], State}.

handle_notify(Msg, State) ->
    {send, [{data, Msg}], State}.
