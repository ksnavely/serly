%%%-------------------------------------------------------------------
%% @doc serly
%%
%% This module provides an abstraction layer for interacting with the
%% application.
%%
%% @end
%%%-------------------------------------------------------------------
-module(serly).

-export([listen/1]).

listen({Module, Function}) ->
    serly_sup:listen({Module, Function}).
