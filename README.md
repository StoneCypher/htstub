htstub
======

The HtStub erlang application webserver.



tl;dr
-----

(todo whargarbl put rebar instructions here as cut-paste in fenced block)



Oh what the hell, another webserver?
------------------------------------

Yep.  This one's simpler.  You can use it call-functionally or Sinatra-style, and since it doesn't do anything, it's fast as the dickens.



Simpler?  Prove it.
-------------------

It's simple enough that you can one-liner it from the REPL.  Fire up an `erl` or a `werl` and try:

```erlang
htstub:serve( fun(_) -> "Hello, world!" end, 8080 ).
```

Now hit [localhost:8080](http://localhost:8080).



That seems overly trivial.
--------------------------

Assuming you're installed and compiled, here's a Sinatra style example, as what many people think of as "a REST server":

```erlang
MyServer = fun
  (get,  <<"">>,            _) -> "Hello, world! (bare)";
  (get,  <<"/">>,           _) -> "Hello, world! (bare root)";
  (get,  <<"/index">>,      _) -> "Hello, world! (index)";
  (get,  <<"/index.html">>, _) -> "Hello, world! (index.html)";
  (post, _,                 _) -> { 501, "No posts, please" };
  (_,    _,                 _) -> { 501, "Not implemented (womp, womp)" }
end.

htstub:rest(MyServer, 8081).
```

Now hit [localhost:8081](http://localhost:8081) in a browser, and you should see your site.



What about route params, you ask?
---------------------------------

Done! We got em!

```erlang
  handler( Params, Req ) ->
    Value = htstub:get_param(Params, id ),
    "You gave me: " ++ Value
  .
  htstub:prest( [ { get, <<"/index/:id">>, _, handler/2 } ], 8082).
```

Now hit [localhost:8081/index/15](http://localhost:8081/index/15) in a browser.



But I want the call to be *(whatever)*
--------------------------------------

This server supports six call notations:

* `:serve` supports `callname(#record)`
* `:rest` supports `callname(method, path, #record)`
* `:prest supports callname( method, path, routeparams, #record )`
* `:xrest` supports `callname(method, path, queryparams, accepttype, #record)`
* `:muxrest` supports `callname(domain, method, path, queryparams, accepttype, #record)`
* `:imuxrest` supports `callname(domain, method, path, queryparams, accepttype, languages, #record)`

It is likely that `:xrest` will be primarily of interest to [HATEOAS](http://timelessrepo.com/haters-gonna-hateoas) developers.  

MuxRest adds multiplexing over domains, and imuxrest handles routing by language accept headers.



Why are you opening port 8080?
------------------------------

On some Unix systems, you might not have access to port 80 without running erlang as root or jumping through hoops, because of some arcane rules about low ports; [it's fixable](#fixinglowports).

But for the getting started tutorial, just use port 8080 instead.

I switch to 8081 so that I don't have to clean up behind myself with `:halt` :smile:



What do you mean by "Application webserver?"
--------------------------------------------

HtStub doesn't want to control how you work.

Some people like a top-level server controlling what's beneath by invokation, `httpd:`/`mochiweb`/`apache` style.  HtStub can do that.  Run a server with a function that directly invokes the things you want done.

Some people want to embed the webserver directly into their application, `yaws`/`appweb`/`KLone` style.  Write your normal app as expected, then call HtStub from inside.  If convenient, use an HtStub handler which communicates with the main application, probably through message passing.

Some people want to provide their webserver as a series of handlers, `cowboy`/`misultin`/`nginx` style.  HtStub can do that.  Treat your work as a series of middlewares which take a call from upstream, possibly work on it, pass it to something downstream, receive a response from downstream, possibly work on it, and pass the response upstream.  From there, you can build a site in a fashion simlar to GoF Chain of Command.

There's probably other ways to do this too.  :smile:

HtStub is not meant to be a standalone product.  It has no access to the filesystem.  It's not a webserver, so much as it is a housing for webserver drivers; hence "application webserver," because it still needs some application to drive its behavior.



And if you don't do rebar?
--------------------------

Ok, we can do that.  You're going to have to suffer through this enormous, agonizing workload:

1. Put the files somewhere.  For these instructions, in `/some/where`.
1. Grab [the other library](https://github.com/StoneCypher/scutil.github.com) too.
1. Start up an erlang console, ostensibly through `erl` or `werl`.
1. `c("/some/where/src/sc.erl").`
1. `c("/some/where/src/htstub.erl").`

Only true wizard shogun super-stars will complete these instructions.  Amazingly, catastrophically difficult, to be sure.




So it's simple?  That's it?
---------------------------

Actually I think simplicity is pretty difficult and pretty valuable.  Being able to stand up a working server within seconds of compiling, with no configuration, seems fairly three one three three seven in my book.

But, no: `htstub` is an odd beast with other merits.  It's stochastically tested using `proper`, and over time should become extensively so.  `htstub` supports the semi-forgotten [URL Matrix Parameter](http://www.w3.org/DesignIssues/MatrixURIs.html) recommendation that Yahoo! and a few novel frameworks seem to like.  `htstub` comes with some middlewares to adapt in functionality.

`htstub` is fast, too.  There are significant pain points as of this writing, which is why no benchmarks yet, but once they're fixed, benches soon.  One of the benefits of doing nothing is how little you have to do to finish.

No-config servers that eat a function are surprisingly useful.  Give it a try; you'll see.



What's this ScUtil dependency
-----------------------------

ScUtil is my big "everything goes here by default" library.  HtStub uses a whole lot of random stuff from inside it.  Same author, same code style, same license.



Docs?  Examples?
----------------

Rebar should have generated them, and they should be in the repo.  But if not:

```erlang
htstub:gen_docs("/path/to/source", "/path/for/output")
```

should generate you a new set.  Example webservers are in `src/examples/`; there's a recommended [order of reading](/src/examples/README.md).



Endnotes
--------

Are you using my webserver?  Please let me know.  I'm curious!

If you want to reach me, try `my github account name at gmail dot com`, please.  Thanks `:)`



Polemic :neckbeard:
-------------------

`htstub` is MIT licensed, because viral licenses and newspeak language modification are evil.  Free is ***only*** free when it's free for everyone.
