-module(serly_client).

-export([start/0, go/2]).

start() ->
    ssl:start(),
    {ok, Socket} = ssl:connect("localhost", 9999, [], infinity),
	ssl:setopts(Socket, [{active, false}]),
    go(Socket, 5).

go(Socket, 0) ->
    ssl:send(Socket, "goodbye"),
    {ok, IOList} = ssl:recv(Socket, 0),
    io:format("Received: ~s~n", [IOList]);
go(Socket, Acc) ->
    ssl:send(Socket, "hello"),
    {ok, IOList} = ssl:recv(Socket, 0),
    io:format("Received: ~s~n", [IOList]),
    go(Socket, Acc - 1).
