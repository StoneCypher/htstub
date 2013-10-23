htstub
======

The HtStub erlang application webserver.

Oh what the hell, another webserver?
------------------------------------

Yep.  This one's simpler.  You can use it call-functionally or Sinatra-style.

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

htstub:serve(MyServer, 8080).
```

Now hit localhost:8080 in a browser, and you should see your site.