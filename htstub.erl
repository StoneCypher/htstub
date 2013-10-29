
%  reading the source?
%  -------------------
%
%  go to a console, and assuming you put your files
%  in a root level directory called "/wherever" ,
%  which is also where you want your docs, type:
%
%  c("/wherever/sc.erl").
%  c("/wherever/htstub.erl").
%  sc:gen_docs("/wherever").
%
%  bang: you now have readable html documentation in /wherever/doc/erl.





%%%%%%%%%%%
%%
%%  @doc HtStub - web development in Erlang as trivial as it <i>could</i> be
%%
%%  This is the 2013 rewrite of the original library.
%%  
%%  <h2>Quick Start</h2>
%%
%%  <dl>
%%    <dt>Step One - Compile the util lib and server lib</dt>
%%    <dd>Assuming "wherever" is the directory in which you've placed the source:<pre>
%%1> c("/wherever/sc.erl").
%%{ok,sc}
%%
%%2> c("/wherever/htstub.erl").
%%{ok,htstub}
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Two - Good to go, try it out (on a high port; see "gotchas" below)</dt>
%%    <dd><pre>
%%3> Handle = htstub:serve( [ {port,23888} ] ).
%%&lt;0.50.0>
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Three - Aaaaand test</dt>
%%    <dd>Pull up your web browser and go to <a href="http://127.0.0.1:23888/" target="_blank">http://127.0.0.1:23888/</a>.</dd>
%%  </dl>
%%
%%  <h2>More Examples</h2>
%%
%%  <dl>
%%    <dt>Step Four - One with your code?</dt>
%%    <dd><pre>
%%3> htstub:halt(Handle).
%%terminate
%%
%%4> MyServer = fun(_) -> "&lt;!doctype html>&lt;html>Y helo thar&lt;/html>" end.
%%#Fun&lt;erl_eval.6.17052888>
%%
%%5> htstub:serve([{handler,MyServer}, {port,3153}]).
%%&lt;0.76.0>
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Five - Aaaaand test</dt>
%%    <dd>Pull up your web browser and go to <a href="http://127.0.0.1:3153/" target="_blank">127.0.0.1:3153</a>.</dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Six - Already set Erlang up for low ports?</dt>
%%    <dd>Cool, then it's even simpler.<pre>
%%1> LowServer = fun(_) -> "&lt;html>&lt;body>Low port WOOOO&lt;/body>&lt;/html>" end.
%%#Fun&lt;erl_eval.6.17052888>
%%
%%2> htstub:serve(LowServer).
%%&lt;0.380.0>
%%</pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Seven - Aaaaand test</dt>
%%    <dd>Try <a href="http://localhost/" target="_blank">http://localhost/</a> this time, just for variety.</dd>
%%  </dl>
%%
%%  <h2>Ridiculous minimalism</h2>
%%
%%  <p>The code below is a complete, compiled, live from RAM webserver.</p>
%%
%%  <h2>Serving a module</h2>
%%
%%  <p>... turns out to be straightforward.</p>
%%
%%  <pre>htstub:serve(   fun modulename:functionname/1   ).</pre>
%%
%%  <h2>Example Servers</h2>
%%
%%  There is a <a href="htstub_servers/index.html" target="_top">growing list</a> of example webservers.
%%
%%  <pre>htstub:serve(   fun(_) -> "&lt;html>&lt;body>Well hello there&lt;/body>&lt;/html>" end   ).</pre>
%%
%%  <h2>Gotchas</h2>
%%  <p>
%%    On most Unix systems, you need to jump through some hoops to open a "low port" (a port below 1024.)  Those hoops
%%    vary system to system, but often involve supervisor escalation, running from a special user with eid, proxying through
%%    iptables, authbind, setcap and capabilities(7), setuid to start as root [sigh], socat, systemd, inetd/xinetd, kernel
%%    modules (!), proxying through apache modules (!!), and other such nonsense.  As usual, Solaris had a reasonable 
%%    response (a config file whitelisting permissions) which nobody remembers.  FreeBSD can get rid of this with the sysctl 
%%    parameters <tt>net.inet.ip.portrange.reservedlow</tt> and <tt>net.inet.ip.portrange.reservedhigh</tt>.  Helpfully, the 
%%    default web port is 80, so hosting a webserver on the standard port falls afoul of this.  <i>Windows doesn't care, so 
%%    Windows devs can happily ignore this</i> (and plan 9 devs and etc.)
%%  </p>
%%
%%  <p>
%%    Also sometimes people just have a webserver running already, and obviously two different things can't accept connections
%%    from the same port.
%%  </p>
%%
%%  <p>
%%    Here's <a href="http://www.staldal.nu/tech/2007/10/31/why-can-only-root-listen-to-ports-below-1024/">an explanation</a>
%%    of the Unix side of this.  You can skip it if you don't care.
%%  </p>
%%
%%  <p>
%%    You will see this with any Erlang daemon - indeed a daemon in any language - and the answer won't vary by what app 
%%    is being run inside.  The error looks like this:
%%  </p>
%%
%%  <pre>
%%1> Server = fun(_) -> "&lt;html>&lt;body>Sup World?&lt;/body>&lt;/html>" end.
%%#Fun&lt;erl_eval.6.17052888>
%%
%%2> htstub:serve(Server).
%%&lt;0.35.0>
%%
%%3>
%%=ERROR REPORT==== 6-Aug-2013::22:55:23 ===
%%Error in process &lt;0.37.0> with exit value: {{badmatch,{error,eacces}},[{htstub,new_listener_loop,4,[{file,"src/htstub.erl"},{line,551}]}]}</pre>
%%
%%  <p>
%%    So when you're getting started, just start with a high port, so you can do this operating system part later, when you aren't 
%%    busy figuring this whole thing out from the ground up.
%%  </p>
%%
%%  <p><tt>htstub:serve([{handler, Server}, {port, 8080}]).</tt></p>
%%
%%  <p>(Thanks <a href="https://github.com/Machinshin">Vat</a>)</p>





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2013 - current John Haugeland, All Rights Reserved
%% @since September July 5, 2013

%% @todo distinguish -opaque types from -types (opaque are not meant to be understood by the outside world, merely tracked, eg handles)

%% @todo module attributes and documentation attributes for license name, license url
%% @todo Every documentation example should be enforced as a unit test, to keep the docs up to date
%% @todo Automate the version back into the docs
%% @todo Automate the version into the .app.src





-module(htstub).

-include_lib("eunit/include/eunit.hrl").





-export([

    test/0,

    lib_version/0,
    running_version/1,
    get_boot_options/1,

    serve/0,
      serve/1,
      serve/2,

    halt/1,

    default_handler/1,

    verbose/1,
    quiet/1,

    int_to_status/1,

    parse_uri/1,

    rest/1,

    standard_datestring/0,



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Private exports, do not use

    loop_upgrade/2

]).





-include("htstub.hrl").





% http://blog.lunatech.com/2009/02/03/what-every-web-developer-must-know-about-url-encoding
% http://doriantaylor.com/policy/http-url-path-parameter-syntax

% comeback todo whargarbl this is naaaaaasty

parse_uri(Uri) when is_binary(Uri) ->

    parse_uri(binary_to_list(Uri));





parse_uri(Uri) ->

    % the return notation in http_uri changes between r14 and r15.  sigh.
    { ok, {Scheme, UserInfo, Host, Port, Path, Query} } = case http_uri:parse(Uri) of
    
        { error, X } -> { error, X };
        { ok,    X } -> { ok,    X };  % r15 or later
        X            -> { ok,    X }   % r14 or earlier :\

    end,



    % first let's fix the username and password up

    [Username, Password] = case sc:explode(":", UserInfo, 2) of

        [[]]    -> [undefined,         undefined];
        [U]     -> [list_to_binary(U), undefined];
        [[],[]] -> [undefined,         undefined];
        [ U,[]] -> [list_to_binary(U), undefined];
        [[], P] -> [undefined,         list_to_binary(P)];
        [ U, P] -> [list_to_binary(U), list_to_binary(P)]

    end,



    PPP = fun(PP) -> % parse path params

        [
            case sc:explode(<<"=">>,list_to_binary(P),2) of
                [Key] -> 
                    {Key};
                [Key,RawVal] ->
                    {Key,sc:explode(<<",">>,RawVal)}
            end
        ||
            P <- PP
        ]

    end,

    [FixedPath, Params] = case sc:explode(";", Path) of
        [[]]  -> [ "", []     ];     
        [T]   -> [  T, []     ];                  % first unique rule is T for paTh, R for paRams
        [T|R] -> [  T, PPP(R) ]
    end,



    FixQueries = fun(QueryFront) ->
        { Single, Multi } = lists:partition(
            fun({_X}) -> true; (_) -> false end,
            [ list_to_tuple(sc:explode("=",Param)) || Param <- sc:explode(";", QueryFront), Param =/= [] ]
        ),
        Single ++ sc:key_bucket(Multi)
    end,

    NoQmQuery = case Query of "" -> ""; Q -> "?" ++ NQ = Q, NQ end,

    [QFront, Fragment] = case sc:explode("#", NoQmQuery, 2) of

        [] ->
            [[],      undefined];

        [IQFront] ->
            [IQFront, undefined];

        [IQFront, IFragment] ->
            [IQFront, list_to_binary(IFragment)]

    end,

    QueryTerms = FixQueries(QFront),




    % todo fixme comeback ugh whargarbl

    BinQT = [
        case QT of
            {Str}      -> {list_to_binary(Str)};
            {Str,List} -> {list_to_binary(Str), [list_to_binary(L) || L <- List]}
        end
    ||
        QT <- QueryTerms
    ],




    OurParse      = #htstub_uri{ 
                        scheme       = Scheme, 
                        user         = Username, 
                        password     = Password, 
                        host         = list_to_binary(Host),
                        port         = Port, 
                        path         = list_to_binary(FixedPath),
                        path_params  = Params,
                        query_params = BinQT,
                        fragment     = Fragment
                     },

    OurParse.





mwrite(false,  _Msg, _Args) ->
    ok;

mwrite(quiet,  _Msg, _Args) ->
    ok;

mwrite(verbose, Msg, Args) ->
    io:format(Msg, Args),
    ok;

mwrite(true,    Msg, Args) ->
    mwrite(verbose, Msg, Args).





return_result(ConnectedSocket, Response) when is_list(Response) ->

    return_result(ConnectedSocket, {200, Response});





return_result(ConnectedSocket, Response) when is_binary(Response) ->

    return_result(ConnectedSocket, {200, binary_to_list(Response)});





return_result(ConnectedSocket, {Status, Response}) when is_integer(Status), is_list(Response) ->

    return_result(ConnectedSocket, {Status, [{"Date",standard_datestring()},{"Content-Type","text/html"},{"Content-Length",integer_to_list(length(Response))}], Response});





return_result(ConnectedSocket, {Status, Headers, Response}) ->

    gen_tcp:send(ConnectedSocket, package_result(Status, Headers, Response)).





package_result(Status, Headers, Response) ->

    "HTTP/1.1 " ++ int_to_status(Status) ++
    lists:flatten([ "\r\n" ++ Field ++ ": " ++ Value || {Field,Value} <- Headers ]) ++
    "\r\n" ++ "\r\n" ++
    Response.





loop_upgrade(Verbose, Handler) ->

    mwrite(Verbose, "upgrading: htstub core loop ~p now version ~p~n~n", [self(), lib_version()]),
    loop(Verbose, Handler).





block_on_parse_http(ConnectedSocket) ->

    { Method, Path, Protocol, Rem } = block_on_http_request(ConnectedSocket, [], []),
    { BodyRem, ProcessedHeaders }   = block_on_http_headers(ConnectedSocket, Rem, [], Path, Protocol, Method),
    parse_body(ConnectedSocket, BodyRem, Path, Protocol, Method, ProcessedHeaders).





parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders) ->

    FinalBodyLength = list_to_integer(proplists:get_value("Content-Length", PHeaders, "0")),
    parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders, size(Body), FinalBodyLength).





parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders, CurrentLength, BodyLength) when CurrentLength < BodyLength ->

    {ok,NewRecv} = gen_tcp:recv(ConnectedSocket, 0),
    parse_body(ConnectedSocket, Body ++ NewRecv, Path, Protocol, Method, PHeaders, CurrentLength+length(NewRecv), BodyLength);





parse_body(_ConnectedSocket,_Body,_Path,_Protocol,_Method,_PHeaders, CurrentLength, BodyLength) when CurrentLength > BodyLength ->

    { error, body_length_longer_than_expected };





parse_body(_ConnectedSocket, Body, Path, Protocol, Method, PHeaders, BodyLength, BodyLength) ->

    body_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength).





body_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength) ->

    { Site, Port } = case proplists:get_value("Host", PHeaders) of

        undefined -> { <<"127.0.0.1">>, 80 };

        Defined ->

            case sc:explode(<<":">>, Defined) of

                [ ISite, IPort ] ->
                    { ISite, IPort };

                [ ISite ] ->
                    { ISite, "" }

            end
    end,

    <<"HTTP/", PVer/binary>> = Protocol,

    [LMajor,  LMinor] = sc:explode(<<".">>, PVer),  % todo harden

    PPath = << 
               <<"http://">>/binary, 
               Site/binary, 
               (if Port == <<"">> -> <<"">>; true -> << <<":">>/binary, (list_to_binary(integer_to_list(Port)))/binary>> end)/binary, 
               Path/binary
            >>,

    { ok, #htstub_request{ 
            request=PPath, 
            http_ver={list_to_integer(binary_to_list(LMajor)),list_to_integer(binary_to_list(LMinor))},
            parsed=parse_uri(PPath), 
            method=Method, 
            pheaders=PHeaders, 
            body=Body, 
            body_length=BodyLength 
          } 
    }.

    % todo this shouldn't just assume http; it could be https, spdy, etc





block_on_http_headers(ConnectedSocket, [], PendingWork, Path, Protocol, Method) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_headers(ConnectedSocket, Data, PendingWork, Path, Protocol, Method);

        { error, E } ->
            { error, E }

    end;





block_on_http_headers(ConnectedSocket, Rem, PendingWork, Path, Protocol, Method) ->

    Unified = PendingWork ++ Rem,

    case sc:explode(<<"\r\n\r\n">>, Unified, 2) of

        [ Headers, BodyRem ] ->
            ProcessedHeaders = [ list_to_tuple(sc:explode(<<": ">>,H)) || H <- sc:explode(<<"\r\n">>, Headers) ],
            { BodyRem, ProcessedHeaders };

        [ NotYet ] ->
            block_on_http_headers(ConnectedSocket, [], NotYet, Path, Protocol, Method)

    end.






block_on_http_request(ConnectedSocket, [], PendingWork) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_request(ConnectedSocket, Data, PendingWork);

        { error, E } ->
            { error, E }

    end;





block_on_http_request(ConnectedSocket, NewWork, PendingWork) ->

    case sc:explode(<<"\r\n">>, PendingWork ++ NewWork, 2) of

        [ReqLine, Rem] ->

            case sc:explode(<<" ">>, ReqLine) of

                [ Method, Path, Protocol ] ->
                    { Method, Path, Protocol, Rem };

                Other ->
                    { error, { malformed_request_line, ReqLine, Other }}

            end;

        [NotYet] ->
            block_on_http_request(ConnectedSocket, [], NotYet)

    end.





handle_new_socket(ConnectedSocket, Handler) ->

    case block_on_parse_http(ConnectedSocket) of

        { ok, Parsed } ->
            return_result(ConnectedSocket, Handler(Parsed)),

            % todo debug this
            % it's not entirely clear why I need to do this, but if i don't, the socket can close before send() pushes its data
            % possibly related to the timeout options i used to set >:(
            receive after 10 -> ok end,

            gen_tcp:close(ConnectedSocket),
            ok;

        { error, _E } ->
            gen_tcp:close(ConnectedSocket),
            ok

    end.





server_listener_loop(ListeningSocket, Handler) ->

    case gen_tcp:accept(ListeningSocket) of

        { ok, ConnectedSocket } ->
            spawn(fun() -> handle_new_socket(ConnectedSocket, Handler) end),
            server_listener_loop(ListeningSocket, Handler);

        { error, closed } ->
            ok;

        { error, _E } ->
            server_listener_loop(ListeningSocket, Handler)

    end.





new_listener_loop(Address, AddressType, Port, Handler, HostRef, Host) ->

    { ok, LSock } = gen_tcp:listen(Port, [binary, {active, false}, {ip, Address}, {packet, raw}, AddressType]),
    Host ! { HostRef, now_listening },
    server_listener_loop(LSock, Handler).





spawn_link_new_listener(Address, AddressType, Port, Handler) ->

    LinkRef = make_ref(),
    Host    = self(),

    Pid = spawn_link(fun() -> new_listener_loop(Address, AddressType, Port, Handler, LinkRef, Host) end),

    receive

        { LinkRef, now_listening } ->
            Pid;

        { LinkRef, {error, E} } ->
            { error, E };

        Other ->
            { error, {misunderstood_message, Other}}

    end.





loop(Verbose, Handler) ->

    mwrite(Verbose, "looping as ~w~n", [self()]),

    receive

        { 'EXIT', FromPid, Reason } ->
            mwrite(Verbose, "TRAPPED AN EXIT: htstub core loop ~p trapped from ~p~n  ~p~n~n", [self(), FromPid, Reason]),
            loop(Verbose, Handler);

        { now_listening_on, _NewPid } ->
            % from auto-listen; discard
            loop(Verbose, Handler);

        { ReqPid, get_running_version } ->
            ReqPid ! { now_running, lib_version() },
            loop(Verbose, Handler);

        { ReqPid, get_boot_options } ->
            mwrite(Verbose, "listing options: htstub core loop ~p~n~n", [self()]),
            ReqPid ! { boot_options, get(boot_options) },
            loop(Verbose, Handler);

        { Ref, Source, terminate } ->
            mwrite(Verbose, "terminating: htstub core loop ~p~n~n", [self()]),
            Source ! { Ref, terminated },
            exit(terminate);

        upgrade ->
            mwrite(Verbose, "upgrading: htstub core loop ~p from version ~p ~n", [self(), lib_version()]),
            htstub:loop_upgrade(Verbose, Handler);

        verbose ->
            mwrite(verbose, "set to verbose: htstub core loop ~p~n~n", [self()]),
            loop(verbose, Handler);

        quiet ->
            mwrite(verbose, "set to quiet: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self()]),
            loop(quiet, Handler);

        Other ->
            mwrite(Verbose, "warning: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self(), Other]),
            loop(Verbose, Handler)

    end.





lib_version() -> 6.





running_version(ServerPid) ->

    ServerPid ! { self(), get_running_version },
    receive
    
        { now_running, V } ->
            V
    
        after 1000 ->
            timeout
    
    end.





get_boot_options(ServerPid) ->

    ServerPid ! { self(), get_boot_options },
    receive
    
        { boot_options, B } ->
            B
    
        after 1000 ->
            timeout
    
    end.





default_handler(Request) ->

    lists:flatten(io_lib:format("<!doctype html><html><head><style type=\"text/css\">p{margin:0;padding:0;}p+p{margin-top:1em;}body{padding:1em;margin:0;font-size:150%;font-family:helvetica,arial,sans-serif;background-color:#cdf;color:#060;}.sig{position:fixed;bottom:1em;right:1em;color:#333;}</style></head><body><p>This is a default page.</p><p>Your webserver is working.</p><div class=\"sig\">Served by <a href=\"http://htstub.com/\">htstub</a>, a micro-webserver for <a href=\"http://erlang.org/\">Erlang</a> made by <a href=\"http://fullof.bs/\">John Haugeland</a>.</div><pre>~n~p~n</pre></body></html>", [Request])).





serve() -> 

    serve(fun default_handler/1).





serve(Handler) when is_function(Handler) -> 

    serve(#htstub_config{handler=Handler});





serve(PropListOptions) when is_list(PropListOptions) -> 

    serve(config_from_plist(PropListOptions));





serve(Options) -> 

    spawn(fun() -> bootstrap_loop(Options) end).





serve(Handler, Port) when is_function(Handler), is_integer(Port), Port >= 0 -> 

    serve(#htstub_config{handler=Handler, port=Port}).





halt(StubPid) ->

    Ref = make_ref(),
    StubPid ! { Ref, self(), terminate },

    io:format("~w ! { ~w, ~w, terminate }~n~n", [StubPid, Ref, self()]),

    receive

        { Ref, terminated } ->
            terminated

    after 5000 ->

        timeout

    end.





bootstrap_loop(Options) ->

    Verbose = Options#htstub_config.verbose,

    mwrite(Verbose, "\\ Entering htstub bootstrap loop as ~w~n", [self()]),

    process_flag(trap_exit, true),
    put(boot_options, Options),

    Handler = Options#htstub_config.handler,
    mwrite(Verbose, " - Setting handler to ~w~n", [Handler]),

    NewPid = spawn_link_new_listener(Options#htstub_config.ip, Options#htstub_config.addrtype, Options#htstub_config.port, Handler),
    mwrite(Verbose, " - spawning: htstub new listener ~w for ~w ~w ~w~n~n", [NewPid, Options#htstub_config.ip, Options#htstub_config.addrtype, Options#htstub_config.port]),

    mwrite(Verbose, " - Bootstrapped! Loop begins as ~p.~n~n", [self()]),
    loop(Verbose, Handler).





verbose(StubPid) ->

    StubPid ! verbose,
    ok.





quiet(StubPid) ->

    StubPid ! quiet,
    ok.






nice_method(<<"GET">>)     -> get;
nice_method(<<"PUT">>)     -> put;
nice_method(<<"POST">>)    -> post;
nice_method(<<"TRACE">>)   -> trace;
nice_method(<<"OPTIONS">>) -> options;
nice_method(<<"HEAD">>)    -> head;
nice_method(<<"DELETE">>)  -> delete;
nice_method(<<"CONNECT">>) -> connect;

nice_method(Other)         -> Other.





restify(RestishHandler) -> 

    fun(Result) -> RestishHandler(nice_method(Result#htstub_request.method), Result#htstub_request.parsed#htstub_uri.path, Result) end.





rest(RestHandler) when is_function(RestHandler) -> 

    serve(restify(RestHandler));





rest(Config) when is_record(Config, htstub_config) -> 

    serve(Config#htstub_config{handler=restify(Config#htstub_config.handler)}).





int_to_status(100) -> "100 Continue";
int_to_status(101) -> "101 Switching Protocols";

int_to_status(200) -> "200 OK";
int_to_status(201) -> "201 Created";
int_to_status(202) -> "202 Accepted";
int_to_status(203) -> "203 Non-Authoritative Information";
int_to_status(204) -> "204 No Content";
int_to_status(205) -> "205 Reset Content";
int_to_status(206) -> "206 Partial Content";

int_to_status(300) -> "300 Multiple Choices";
int_to_status(301) -> "301 Moved Permanently";
int_to_status(302) -> "302 Found";
int_to_status(303) -> "303 See Other";
int_to_status(304) -> "304 Not Modified";
int_to_status(305) -> "305 Use Proxy";
int_to_status(306) -> "306 (Unused)";
int_to_status(307) -> "307 Temporary Redirect";

int_to_status(400) -> "400 Bad Request";
int_to_status(401) -> "401 Unauthorized";
int_to_status(402) -> "402 Payment Required";
int_to_status(403) -> "403 Forbidden";
int_to_status(404) -> "404 Not Found";
int_to_status(405) -> "405 Method Not Allowed";
int_to_status(406) -> "406 Not Acceptable";
int_to_status(407) -> "407 Proxy Authentication Required";
int_to_status(408) -> "408 Request Timeout";
int_to_status(409) -> "409 Conflict";
int_to_status(410) -> "410 Gone";
int_to_status(411) -> "411 Length Required";
int_to_status(412) -> "412 Precondition Failed";
int_to_status(413) -> "413 Request Entity Too Large";
int_to_status(414) -> "414 Request-URI Too Long";
int_to_status(415) -> "415 Unsupported Media Type";
int_to_status(416) -> "416 Requested Range Not Satisfiable";
int_to_status(417) -> "417 Expectation Failed";

int_to_status(500) -> "500 Internal Server Error";
int_to_status(501) -> "501 Not Implemented";
int_to_status(502) -> "502 Bad Gateway";
int_to_status(503) -> "503 Service Unavailable";
int_to_status(504) -> "504 Gateway Timeout";
int_to_status(505) -> "505 HTTP Version Not Supported".





standard_datestring() ->

    { {Y,M,D}, {H,Mn,S} } = erlang:universaltime(),

    MonthLabel = element(M, {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    Day = element(calendar:day_of_the_week(Y,M,D), {"Mon","Tue","Wed","Thu","Fri","Sat","Sun"}),

    lists:flatten(io_lib:format("~s, ~b ~s ~b ~2.10.0b:~2.10.0b:~2.10.0b GMT",[Day,D,MonthLabel,Y,H,Mn,S])).





config_from_plist(PList) ->

    config_from_plist(#htstub_config{}, PList).





config_from_plist(Config, []) ->

    Config;





config_from_plist(Config, PList) ->

    [ PItem | PRem ] = PList,

    case PItem of

        { ip, NIP } ->
            config_from_plist(Config#htstub_config{ ip=NIP }, PRem);

        { addrtype, NAT } ->
            config_from_plist(Config#htstub_config{ addrtype=NAT }, PRem);

        { port, NPT } ->
            config_from_plist(Config#htstub_config{ port=NPT }, PRem);

        quiet ->
            config_from_plist(Config#htstub_config{ verbose=quiet }, PRem);

        verbose ->
            config_from_plist(Config#htstub_config{ verbose=verbose }, PRem);

        { verbose, NVB } ->
            config_from_plist(Config#htstub_config{ verbose=NVB }, PRem);

        { server_name, NSN } ->
            config_from_plist(Config#htstub_config{ server_name=NSN }, PRem);

        { handler, NHN } ->
            config_from_plist(Config#htstub_config{ handler=NHN }, PRem);

        { middleware, NMW } ->
            config_from_plist(Config#htstub_config{ middleware=NMW }, PRem);

        Other ->
            { error, { unrecognized_config, Other }}

    end.





%% @doc (not testworthy) Runs the test suite in terse form. ``` c("wherever/htstub.erl").
%% {ok,htstub}
%%
%% 2> c("wherever/htstub_tests.erl").
%% {ok,htstub_tests}
%%
%% 3> htstub:test().
%%   All 9 tests passed.
%% ok'''
%%
%% @since 2.0.3

-spec test() -> 
    ok   | 
    error.

test() ->

    eunit:test(htstub).
