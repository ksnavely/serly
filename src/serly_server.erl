%%%-------------------------------------------------------------------
%% @doc serly_server
%%
%% Workers here will accept a TLS TCP connection and pass an active socket
%% to a callback module and function. The socket properties and callback
%% are passed in via a server_state record.
%%
%% The socket acceptance process is blocking. So that we don't end up blocking
%% the business logic, a new serly_sup worker is created
%% to accept another connection before calling the business logic loop.
%%
%% @end
%%%-------------------------------------------------------------------

-module(serly_server).

-include("serly.hrl").

-behavior(gen_server).

%% OTP exports
-export([init/1, handle_cast/2, terminate/2]).

%% Non-OTP exports
-export([start_link/1, handle_connection/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    % Tell the new server to accept a connection
    gen_server:cast(Pid, {accept, self()}),
    {ok, Pid}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(State) ->
    {ok, State}.

handle_cast({accept, _Pid}, State) ->
    % This callback will process the blocking part of
    % connection acceptance, then start a new serly_server
    % to process then next 
    #server_state{ssl_sock = Socket} = State,
    handle_connection(ssl:transport_accept(Socket), State).

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
