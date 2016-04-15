[![Stories in Ready](https://badge.waffle.io/inaka/lasse.png?label=ready&title=Ready)](https://waffle.io/inaka/lasse)
# Lasse

<img src="http://i61.tinypic.com/40pkl.jpg" align="right" style="float:right" height="400" />

SSE handler for Cowboy.

### References

* [Cowboy](/extend/cowboy)
* [SSE](http://dev.w3.org/html5/eventsource/)
* [canillita's handler](/canillita/blob/master/src/canillita_news_handler.erl) (as a reference)

### Usage

``lasse`` provides a [cowboy loop handler](http://ninenines.eu/docs/en/cowboy/HEAD/guide/loop_handlers/)
called ``lasse_handler`` that describes a behaviour. To include it in your server routes, just add
the following tuple to your [dispatch routes](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/):

```erlang
{<<"/your/[:route]">>, lasse_handler, [your_module]}
% or
{<<"/your/[:route]">>, lasse_handler, [{module, your_module}, {init_args, Args}]}
```

Specifying the ``module`` (e.g ``your_module``) is mandatory while providing a value for ``init_args``
is optional.

Additionally, in your module, you have to implement the ``lasse_handler`` behaviour and its
[callbacks](#callbacks):

```erlang
-behaviour(lasse_handler).
```

### Contact Us

For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/lasse/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

### Examples

You can find some example applications that implement the ``lasse_handler`` in the ``examples`` folder.

Running the examples is as simple as executing ``make run``, given you have the ``make`` tool, ``git``
and ``erlang`` installed in your environment.

## API

### notify(Pid, Message) -> ok

Used to send in-band messages to the handler given its Pid.

Types:

- Pid = pid()
- Message = any()

<a name="callbacks"></a>
## Callbacks

### init(LastEventId, Req) -> {ok, NewReq, State}
    | {ok, NewReq, InitialEvents, State}
    | {no_content, NewReq, State}
    | {shutdown, StatusCode, Headers, Body, NewReq, State}

Will be called upon initialization of the handler, receiving the value of the ``"last-event-id"`` header
if there is one or ``undefined`` otherwise. If everything goes well it should return
``{ok, NewReq, State}`` to leave the connection open or ``{ok, NewReq, InitialEvents, State}`` if there are
any ``InitialEvents`` that need to ben sent before the handler enters its loop.

In case the handler has no content to deliver it should return ``{no_content, NewReq, State}`` and the client
will receive a response with a status code ``204 No Content``. A custom response can be provided for other
scenarios by returning ``{shutdown, StatusCode, Headers, Body, NewReq, State}``, which will cause the handler
to reply to the client with the information supplied and then terminate.

Types:

- InitArgs = any()
- LastEventId = binary() | undefined
- Req = cowboy_req:req()
- NewReq = cowboy_req:req()
- State = any()
- StatusCode = cowboy:http_status()
- Headers = cowboy:http_headers()
- Body = iodata()

### handle_notify(Msg, State) -> Result

Receives and processes in-band messages sent through the ``lasse_handler:notify/2`` function.

Types:

- Msg = any()
- State = any()
- Result = [result()](#result_type)

### handle_info(Msg, State) -> Result

Receives and processes out-of-band messages sent directly to the handler's process.

Types:

- Msg = any()
- State = any()
- Result = [result()](#result_type)

### handle_error(Msg, Reason, State) -> NewState

If there's a problem while sending a chunk to the client, this function will be called after which the handler will terminate.

Types:

- Msg = any()
- Reason = atom()
- State = any()
- NewState = any()

### terminate(Reason, Req, State) -> ok

This function will be called before terminating the handler, its return value is ignored.

Types:

- Reason = atom()
- Req = cowboy:http_headers()
- State = any()

## Types

<a name="result_type"></a>
### result() = {'send', Event :: event(), NewState :: any()}
    | {'nosend', NewState :: any()}
    | {'stop', NewState :: any()}

### event() = [event_value(), ...]

The field ``data`` is required for every event returned by ``handle_notify()`` and ``hanfle_info()``,
if none is supplied ``data_required`` will be thrown.

### event_value() = {'id', binary()}
    | {'event', binary()}
    | {'data', binary()}
    | {'retry', binary()}
    | {'comment', binary()}
