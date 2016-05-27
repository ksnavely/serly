-module(usage_example).

-export([start/0, client/2, server/1, handle_connection/1, handle_recvd/2]).

start() ->
    application:start(serly),
    % Plug in our business logic loop/server
    serly_sup:listen({usage_example, server}),

    % Start the example client interaction
    ssl:start(),
    {ok, Port} = application:get_env(serly, port),
    handle_connection(
        ssl:connect("localhost", Port, [], infinity)
    ).

handle_connection({error, Error}) ->
    {error, Error};
handle_connection({ok, Socket}) ->
	ssl:setopts(Socket, [{active, false}]),
    client(Socket, 5).

%%====================================================================
%% Example client business logic
%%====================================================================

client(Socket, 0) ->
    ssl:send(Socket, "goodbye"),
    {ok, IOList} = ssl:recv(Socket, 0),
    io:format("Received: ~s~n", [IOList]);
client(Socket, Acc) ->
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
