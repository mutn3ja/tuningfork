%%%-------------------------------------------------------------------
%% @doc tuningfork public API
%% @end
%%%-------------------------------------------------------------------

-module(tuningfork_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tuningfork_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
