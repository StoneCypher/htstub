
-module(hello_world_ws).





-export([
    page/1
]).





%%%%%%%%% 
%%
%%  @doc Returns a "hello, world" webpage.
%%
%%  This is an intentionally trivial webserver driver.  This completely ignores the request and
%%  returns a string constant.  (htstub interprets string constants as `200, ok`.)
%%
%%  This is meant as first steps with htstub; usage is kept simple.  
%%
%%  ```1> htstub:serve( fun hello_world_ws:page/1 ).
%%  <0.45.0>'''
%%
%%  That's it.  There's now <a href="http://localhost/">a webserver running</a>, assuming you have 
%%  rights to port 80.  (If not, `1> htstub:serve( fun hello_world_ws:page/1, 9717 )` or whatever.)


page( _ ) ->

    "<!doctype html><html><body>Hello, world!</body></html>".
