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
-export([init/1, handle_cast/2, terminate/2]).

%% Non-OTP exports
-export([start/1, handle_connection/2]).

%%====================================================================
%% API
%%====================================================================

start(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    gen_server:cast(Pid, {accepting, self()}),
    {ok, Pid}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(State) ->
    {ok, State}.

handle_cast({accepting, _Pid}, State) ->
    % Do the blocking part of socket acceptance
    #server_state{ssl_sock = Socket} = State,
    handle_connection(ssl:transport_accept(Socket), State).

terminate(Reason, State) ->
    ok.

handle_connection({error, Error}, State) ->
    {stop, Error, State};
handle_connection({ok, Socket}, State) ->
    % TLS/SSL handshake
    ok = ssl:ssl_accept(Socket),

    % A connection has been accepted, begin a new serly_sup
    % child process to handle the next connection while we run
    % the business logic.
    supervisor:start_child(serly_sup, [State]),


    % Work with the connection
    #server_state{loop = {M, F}} = State,
    M:F(Socket),

    {stop, normal, State}.
