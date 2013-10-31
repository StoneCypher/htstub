
-module(hello_world_ws).





-export([
    page/1
]).





%%%%%%%%% 
%%
%%  @doc Returns a "hello, world" webpage.
%%
%%  This is an intentionally trivial webserver driver.  This is essentially the simplest that it gets:
%%  a rendering function that completely ignores the request and returns a trivial fixed string.  When
%%  passed a string, htstub will assume that the request is `200 OK` and return it as such.  
%%
%%  This is meant as someone's first steps with htstub, so usage is kept simple.  ```1> htstub:serve( fun hello_world_ws:page/1 ).
%%  <0.45.0>'''
%%
%%  That's it.  There's now <a href="http://localhost/">a webserver running</a>, assuming you have 
%%  rights to port 80 and it isn't already in use.  If 80 is unavailable, try this instead: ```1> htstub:serve( fun hello_world_ws:page/1, 9717 ).
%%  <0.45.0>'''
%%
%%  Now you should have <a href="http://localhost:9717/">a webserver on 9717</a> instead.  (If 9717 is 
%%  also in use, you'll have to pick a number of your own.)

page( _ ) ->

    "<!doctype html><html><body>Hello, world!</body></html>".
