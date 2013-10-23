htstub
======

The HtStub erlang application webserver.

Oh what the hell, another webserver?
------------------------------------

Yep.  This one's simpler.  You can use it call-functionally or Sinatra-style, and since it doesn't do anything, it's fast as the dickens.

Simpler?  Prove it.
-------------------

```
htstub:serve( fun(_) -> "Hello, world!" end ).
```

That seems overly trivial.
--------------------------

Assuming you're installed and compiled, here's a Sinatra style example, as what many people think of as "a REST server":

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

Now hit [localhost:8080](http://localhost:8080) in a browser, and you should see your site.

But I want the call to be (whatever)
------------------------------------

This server supports three call notations:

* `:serve` supports `callname(#record)`
* `:rest` supports `callname(method, path, #record)`
* `:xrest` supports `callname(method, path, queryparams, accepttype, #record)`

It is likely that `:xrest` will be primarily of interest to [HATEOAS](http://timelessrepo.com/haters-gonna-hateoas) developers.

Application webserver?
----------------------

HtStub doesn't want to control how you work.

Some people like a top-level server controlling what's beneath by invokation, `httpd`/`mochiweb` style.  HtStub can do that.

Some people want to embed the webserver directly into their application, `yaws` style.

Some people want to provide their webserver as a series of handlers, `cowboy`/`misultin` style.  HtStub can do that.

There's probably other ways to do this too.  :smile:

"Assuming you're installed," he says.
-------------------------------------

There should be `rebar` instructions here, but I haven't written the `rebar` file yet.  (d'oh)

And if you don't do rebar?
--------------------------

Ok, we can do that.  You're going to have to suffer through this enormous, agonizing workload:

1. Put the files somewhere.  For these instructions, in `/some/where`.
1. Grab [the other library](https://github.com/StoneCypher/scutil.github.com) too.
1. Start up an erlang console, ostensibly through `erl` or `werl`.
1. `c("/some/where/src/sc.erl").`
1. `c("/some/where/src/htstub.erl").`

Only true wizard shogun super-stars will complete these instructions.  Amazingly, catastrophically difficult, to be sure.

What's this ScUtil dependency
-----------------------------

ScUtil is my big "everything goes here by default" library.  HtStub uses a whole lot of random stuff from inside it.  Same author, same code style, same license.