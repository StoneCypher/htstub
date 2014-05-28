-module(testing).

-compile([export_all]).

-include("../include/htstub.hrl").


handler(Params, Req ) ->
    Value = htstub:get_param( Params, id ),
    io:format("params ~p Request = ~p~n Value = ~p~n", [Params, Req, Value]),
    %{ 200, [],  }
    %{ 200, [], io_lib:format("Request: ~p", [Req]) }
    "You gave me: " ++ Value
.

routes(  ) ->
    Args = [],
    [
        { get, <<"/api_1/u/:id">>, Args, fun testing:handler/2   }
    ].
%,
%H = fun(Params, Req) -> Value = htstub:get_param( Params, id ), "You gave me" ++ Value end.
%A =     [ { get, <<"/api_1/u/:id">>, [], H } ] .

%htstub:prest(A, 8081).

test() ->
    htstub:prest(routes(), 8081)
.

foo() ->
    R1 = "/api_1/u/:id",
    R2 = "/api_1/u/:id/finance/:tax_id",
    R3 = "/api_1/u/:id/features/:feature_id/finance/:tax_id",
    io:format( " 1 => ~p~n", htstub:parse_route_for_params( R1  ) )
    ,
    io:format( " 2 => ~p~n", [ htstub:parse_route_for_params( R2 ) ] )
    ,
    io:format( " 3 => ~p~n", [ htstub:parse_route_for_params( R3 ) ] )
    ,
    U1 = "/api_1/u/15",
    U2 = "/api_1/u/15/finance/110",
    U3 = "/api_1/u/15/features/110/finance/50",
    io:format( "u1 => ~p u1 => ~p~n", [ R1, htstub:parse_url_for_params( R1, U1 )] )
    ,
    io:format( "u2 => ~p  u2 => ~p~n", [ R2, htstub:parse_url_for_params( R2, U2 )] )
    ,
    io:format( "u3 => ~p  u3 => ~p~n", [ R3, htstub:parse_url_for_params( R3, U3 )] )
    ,
    io:format( "u3 => ~p  u3 => ~p~n", [ R3, htstub:parse_url_for_params( R2, U3 )] )
%    ,
.

