%%%-------------------------------------------------------------------
%% @doc serly top level supervisor.
%%
%%  serly_sup provides simple_one_for_one supervision of serly_server
%%  children/workers. Workers are add dynamically by the listen/1
%%  API exposed here, and by the workers themselves.
%%
%% @end
%%%-------------------------------------------------------------------

-module(serly_sup).

-include("serly.hrl").

-behaviour(supervisor).

%% API
-export([listen/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% listen
%% Open up a socket given a configured port, x509 certificate, and key
%% Start a new worker under serly_sup which will handle incoming connections
listen(Loop = {M, F}) ->
    ssl:start(),
    {ok, Port} = application:get_env(serly, port),
    {ok, CertFile} = application:get_env(serly, certfile),
    {ok, KeyFile} = application:get_env(serly, keyfile),

    % Open up a socket for binary data
    Returned = ssl:listen(
        Port,
        [
            {mode, binary},
            {certfile, CertFile},
            {keyfile, KeyFile},
            {reuseaddr, true}, % we'll spawn many processes using the socket
            {active, false}  % use ssl:recv statements to get data
        ]
    ),
    case Returned of
        {ok, Socket} ->
			State = #server_state{port = Port, loop = Loop, ssl_sock = Socket},
			supervisor:start_child(?MODULE, [State]);
        {error, Error} ->
            {error, Error}
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    % We'll use a simple_one_for_one strategy and dynamic children
    Children = [{
        serly_server,              % Id
        {serly_server, start, []}, % {Module, Function, Arguments}
        temporary,                 % RestartStrategy
        brutal_kill,               % ShutdownStrategy
        worker,                    % worker or supervisor
        [serly_server]             % ModuleList which implements the process
    }],

    % {ok, {{RestartStrategy, AllowedRestarts, MaxSeconds}, ChildSpecificationList}}
    {ok, {{simple_one_for_one, 5, 10}, Children}}.
