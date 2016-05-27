%%%-------------------------------------------------------------------
%% @doc usage_example
%%
%% This module provides a simple example of using the serly application
%% to server up TLS/SSL TCP connections.
%%
%% @end
%%%-------------------------------------------------------------------

-module(usage_example).

-export([start/0, multi_start/1, client/2, server/1, handle_connection/1, handle_recvd/2]).

%%====================================================================
%% Example entry points
%%====================================================================

%% start
%%
%% See multi_start/1
start() ->
    multi_start(1).

%% multi_start
%%
%% Start the serly application and pass a callback {Module, Function}
%% to serly:listen.
%%
%% serly will handle the TLS TCP connection serving and pass the active
%% socket to the callback function.
%%
%% spawn NClients client sessions concurrently
multi_start(NClients) ->
    % The following two lines are all that's needed to use serly!
    application:start(serly),
    serly:listen({usage_example, server}),

    % Start the example client(s)
    spawn_client(NClients).

spawn_client(0) ->
    ok;
spawn_client(Acc) ->
    spawn(
        fun() ->
			ssl:start(),
			{ok, Port} = application:get_env(serly, port),
			handle_connection(
				ssl:connect("localhost", Port, [], infinity)
			)
         end
    ),
    spawn_client(Acc - 1).

handle_connection({error, Error}) ->
    {error, Error};
handle_connection({ok, Socket}) ->
	% Implies client usage of ssl:recv to receive messages
	ssl:setopts(Socket, [{active, false}]),
    client(Socket, 5).

%%====================================================================
%% Example client business logic
%%====================================================================

client(Socket, 0) ->
    % Say "goodbye" -- server will close the socket
    ssl:send(Socket, "goodbye"),
    {ok, IOList} = ssl:recv(Socket, 0),
    io:format("Received: ~s~n", [IOList]);
client(Socket, Acc) ->
    % Send "hello" Acc times
    ssl:send(Socket, "hello"),
    {ok, IOList} = ssl:recv(Socket, 0),
    io:format("Received: ~s~n", [IOList]),
    client(Socket, Acc - 1).

%%====================================================================
%% Example server business logic
%%====================================================================

server(Socket) ->
    case ssl:recv(Socket, 0) of
        {ok, IOList} ->
            handle_recvd(iolist_to_binary(IOList), Socket),
            server(Socket);
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
