serly
=====

serly is an OTP application which serves up TLS TCP sockets
to a callback function. The application config, serly.config,
details which port and x509 certificate/key to use for the
created sockets.

This application is pretty new so it may not handle all socket
closing/error modes perfectly.

Build
-----
serly was developed against Erlang 18.3. It hasn't been tested with earlier
versions, but it shouldn't rely on 'recent' language changes/features.
Let me know if it builds for you and on what Erlang version. :)

    $ rebar3 compile

Usage
-----

A key principle in serly is its plugin behavior -- the consumer
provides a {Module, Function} callback to serly:listen/1. serly
will prepare a socket and pass it to the callback. The callback
should serve as an entry point for the business logic.

To see an example of this, check out the `usage_example` module:

```
$> rebar3 compile
===> Verifying dependencies...
===> Compiling serly
# You'll see some warnings for undefined gen_server callbacks

$> erl -pa $(rebar3 path) -config serly.config
1> usage_example:start().
Received: Why, hello there!
Received: Why, hello there!
Received: Why, hello there!
Received: Why, hello there!
Received: Why, hello there!
Received: Y'all come back now!
ok
```
