htstub
======

The HtStub erlang application webserver.

Oh what the hell, another webserver?
------------------------------------

Yep.  This one's simpler.  You can use it call-functionally or Sinatra-style, and since it doesn't do anything, it's fast as the dickens.

Assuming you're installed and compiled, here's a Sinatra style example:

```
MyServer = fun
  (get,  "",            _) -> "Hello, world! (bare)";
  (get,  "/",           _) -> "Hello, world! (bare root)";
  (get,  "/index",      _) -> "Hello, world! (index)";
  (get,  "/index.html", _) -> "Hello, world! (index.html)";
  (post, _,             _) -> { 501, "No posts, please" };
  (_,    _,             _) -> { 501, "Not implemented (womp, womp)" }
end.

htstub:rest(MyServer, 8080).
```

Now hit (http://localhost:8080)[localhost:8080] in a browser, and you should see your site.

"Assuming you're installed," he says.
-------------------------------------

There should be `rebar` instructions here, but I haven't written the `rebar` file yet.  (d'oh)

And if you don't do rebar?
--------------------------

Ok, we can do that.  You're going to have to suffer through this enormous, agonizing workload:

1. Put the files somewhere.  For these instructions, in `/some/where`.
1. Grab (https://github.com/StoneCypher/scutil.github.com)[the other library] too.
1. Start up an erlang console, ostensibly through `erl` or `werl`.
1. `c("/some/where/src/sc.erl").`
1. `c("/some/where/src/htstub.erl").`

Only true wizard shogun super-stars will complete these instructions.  Amazingly, catastrophically difficult, to be sure.

What's this ScUtil dependency
-----------------------------

ScUtil is my big "everything goes here by default" library.  HtStub uses a whole lot of random stuff from inside it.  Same author, same code style, same license.