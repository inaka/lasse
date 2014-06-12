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
    case lists:keyfind(<<"process-name">>, 1, Headers) of
        {<<"process-name">>, ProcNameBin} ->
            ProcName = binary_to_term(ProcNameBin),
            lager:info("Initiating an ~p in ~p", [ProcName, whereis(ProcName)]),
            register(ProcName, self());
        _ ->
            lager:info("Initiating handler"),
            ok
    end,

    {ok, Req, {}}.

handle_info(Msg, State) ->
    {send, [{data, Msg}], State}.

handle_notify(Msg, State) ->
    {send, [{data, Msg}], State}.
