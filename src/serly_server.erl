%% TODO based on http://20bits.com/article/erlang-a-generalized-tcp-server
% serly_server
%
% The idea here is that the gen_server will create a new SSL socket during
% init, and once the socket is set up, jump into a loop with the socket as the
% state. The loop should recieve messages from the socket and process them as
% cherl server does now.

% Some part of the socket acceptance process is likely blocking. In order to
% prevent blocking of loop execution, spawn a new
-module(serly_server).

-behavior(gen_server).

%% OTP exports
-export([init/1, handle_cast/2]).

%% Non-OTP exports
-export([start/3, accept/1, accept_loop/1, ssl_server/1, handle_recvd/2, test/0]).

-record(
    server_state,
    {
        host,
        port,
        loop,
        ssl_sock
    }
).

start(Module, Port, Loop) ->
    ssl:start(),
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Module}, ?MODULE, State, []).

%% OTP gen_server Behavior

init(State = #server_state{port = Port, loop = Loop}) ->
    % Grab the cert/key info
    CertFile = "/home/ksnavely/programming/localhost-certs/localhost.crt",
    KeyFile = "/home/ksnavely/programming/localhost-certs/localhost.key",

    % Open up a socket for binary data
    {ok, Socket} = ssl:listen(9999, [{mode, binary}, {certfile, CertFile}, {keyfile, KeyFile}, {reuseaddr, true}, {active, false}]),
    State2 = State#server_state{ssl_sock = Socket},

    % Kick off an initial connection
    {ok, accept(State2)}.

handle_cast({accepted, _Pid}, State) ->
    {noreply, accept(State)}.

%% Non-OTP functions

accept(State = #server_state{ssl_sock = Socket, loop = Loop}) ->
    % Process the rest of the socket acceptance in another process
    proc_lib:spawn(?MODULE, accept_loop, [{self(), Socket, Loop}]),
    State.

accept_loop({Pid, Socket, {M, F} = Loop}) ->
    % Do the blocking part of socket acceptance
    {ok, Socket2} = ssl:transport_accept(Socket),
    ok = ssl:ssl_accept(Socket2),

    % We've accepted, cast to trigger this accept_loop again
    gen_server:cast(Pid, {accepted, self()}),

    % Work with the connection
    M:F(Socket2).

ssl_server(Socket) ->
    case ssl:recv(Socket, 0) of
        {ok, IOList} -> 
            handle_recvd(iolist_to_binary(IOList), Socket),
            ssl_server(Socket);
        {error, closed} -> ok
    end.

handle_recvd(<<"hello">>, Socket) ->
    ssl:send(Socket, "Why, hello there!");
handle_recvd(<<"goodbye">>, Socket) ->
    ssl:send(Socket, "Y'all come back now!"),
    ssl:close(Socket);
handle_recvd(_, Socket) ->
    io:format("Server received unexpected handle_recvd arg!~n", []),
    ssl:close(Socket).

test() ->
    start(?MODULE, 9999, {?MODULE, ssl_server}).
