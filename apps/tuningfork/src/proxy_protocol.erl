-module(proxy_protocol).
-behaviour(gen_statem).
-behaviour(ranch_protocol).

%% API
-export([start_link/3]).

%% gen_statem
-export([callback_mode/0]).
-export([init/1]).
-export([connected/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(TIMEOUT, 60000).

-record(state, {ref, transport, client_socket, server_socket}).

%% API

start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

%% gen_statem

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
    {ok, connected, #state{ref = Ref, transport = Transport}, ?TIMEOUT}.

connected(
    enter,
    connected,
    StateData = #state{
        ref = Ref, transport = Transport
    }
) ->
    {ok, ClientSocket} = ranch:handshake(Ref),
	UpstreamHostAddr = s:getenv("TUNINGFORK_UPSTREAM_HOST_ADDR", "google.com"),
	{ok, ServerSocket} = gen_tcp:connect(UpstreamHostAddr, 443, [binary, {active, once}, {packet, 0}]),
    ok = Transport:setopts(ClientSocket, [binary, {active, once}, {packet, 0}]),
    {keep_state, StateData#state{client_socket = ClientSocket, server_socket = ServerSocket}};
connected(
    info,
    {tcp, ClientSocket, Data},
    _StateData = #state{
        client_socket = ClientSocket, server_socket = ServerSocket, transport = Transport
    }
) when
    byte_size(Data) >= 1
->
    Transport:setopts(ClientSocket, [{active, once}]),
	ok = gen_tcp:send(ServerSocket, Data),
    {keep_state_and_data, ?TIMEOUT};
connected(
    info,
    {tcp, ServerSocket, Data},
    _StateData = #state{
        client_socket = ClientSocket, server_socket = ServerSocket, transport = Transport
    }
) when
    byte_size(Data) >= 1
->
    inet:setopts(ServerSocket, [{active, once}]),
	Transport:send(ClientSocket, Data),
    {keep_state_and_data, ?TIMEOUT};
connected(info, {tcp_closed, _Socket}, _StateData) ->
    {stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
    {stop, Reason};
connected({call, From}, _Request, _StateData) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
connected(cast, _Msg, _StateData) ->
    keep_state_and_data;
connected(timeout, _Msg, _StateData) ->
    {stop, normal};
connected(_EventType, _Msg, _StateData) ->
    {stop, normal}.

terminate(
    _Reason,
    _StateName,
    _StateData = #state{
        client_socket = ClientSocket, server_socket = ServerSocket, transport = Transport
    }
)
->
    catch Transport:close(ClientSocket),
	catch Transport:close(ServerSocket),
	ok;
terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
