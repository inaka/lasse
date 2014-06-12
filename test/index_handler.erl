-module(index_handler).

-export([
         init/3,
         info/3,
         terminate/3,
         handle/2
        ]).

init(_Transport, Req, _Opts) ->
    Headers = [
               {<<"content-type">>, <<"text/html">>}
              ],
    Body = <<"
<script>
var x = undefined;
function create() {
  if(x != undefined) {
      alert('Close it first!');
      return;
  }
  x = new EventSource('/events');
  x.onmessage = function(e) { console.log(e); };
};
function closeSource() {
  if(x == undefined) {
      alert('Create it first!');
      return;
  }
  x.close();
  x = undefined;
};
</script>
<body>
<button id='create' onclick='create()'>Create</button>
<button id='close' onclick='closeSource()'>Close</button>
</body>
">>,
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, {}}.

handle(Req, State) ->
    {ok, Req, State}.

info(_Msg, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.


