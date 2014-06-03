# lasse
## SSE handler for Cowboy

### References
* [Cowboy](/extend/cowboy)
* [SSE](http://dev.w3.org/html5/eventsource/)
* [canillita's handler](/canillita/blob/master/src/canillita_news_handler.erl) (as a reference)

### How to use it
``lasse`` provides a [cowboy loop handler](http://ninenines.eu/docs/en/cowboy/HEAD/guide/loop_handlers/) called ``lasse_handler`` that describes a behaviour. To include it in your server routes, just add the following tuple to your [dispatch routes](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/):

```erlang
{<<"/your/[:route]">>, lasse_handler, [your_module]}
```

And, in your module, you have to implement the following behaviour:

```erlang
-behaviour(lasse_handler).
```
