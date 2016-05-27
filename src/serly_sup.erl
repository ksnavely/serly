%%%-------------------------------------------------------------------
%% @doc serly top level supervisor.
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

%%====================================================================
%% Internal functions
%%====================================================================

listen(Loop = {M, F}) ->
    ssl:start(),
    Port = 9999,
    CertFile = "/home/ksnavely/programming/localhost-certs/localhost.crt",
    KeyFile = "/home/ksnavely/programming/localhost-certs/localhost.key",

    % Open up a socket for binary data
    % TODO handle {error,eaddrinuse}
    {ok, Socket} = ssl:listen(Port, [{mode, binary}, {certfile, CertFile}, {keyfile, KeyFile}, {reuseaddr, true}, {active, false}]),

    State = #server_state{port = Port, loop = Loop, ssl_sock = Socket},
    supervisor:start_child(?MODULE, [State]).
