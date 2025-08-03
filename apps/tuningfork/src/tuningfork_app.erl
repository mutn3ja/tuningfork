%%%-------------------------------------------------------------------
%% @doc tuningfork public API
%% @end
%%%-------------------------------------------------------------------

-module(tuningfork_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} =
        ranch:start_listener(tuningfork,
                             ranch_tcp,
                             #{socket_opts => [{port, 443}]},
                             proxy_protocol,
                             []),
    tuningfork_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
