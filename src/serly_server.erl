%
% serly_server
%
% The idea here is that the gen_server will create a new SSL socket during
% init, and once the socket is set up, jump into a loop with the socket as the
% state. The loop should recieve messages from the socket and process them.

% The socket acceptance process is blocking. So that we don't end up blocking
% the business logic, we spawn a new process to accept another connection before
% calling the business logic loop.

-module(serly_server).

-include("serly.hrl").

-behavior(gen_server).

%% OTP exports
-export([init/1, handle_cast/2]).

%% Non-OTP exports
-export([start/1, accept/1, accept_loop/1, ssl_server/1, handle_recvd/2]).


start(State) ->
    gen_server:start_link(?MODULE, State, []).

%% OTP gen_server Behavior

init(State) ->
    % Kick off an initial connection
    State2 = accept(State),
    {ok, State2}.

handle_cast({accepted, _Pid}, State) ->
    % A connection has been accepted, accept a new one.
    State2 = accept(State),
    {noreply, State2}.

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
