%% The server_state record serves as the state for the
%% serly_server gen_server
-record(
    server_state,
    {
        port,
        loop,
        ssl_sock
    }
).
