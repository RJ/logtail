-module(logtail_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:start(cowboy),
    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
            {[<<"websocket">>], logtail_web, []},
            {['...'], cowboy_http_static, [
                {directory, {priv_dir, logtail, []}},
                {mimetypes, [
                    {<<".css">>, [<<"text/css">>]},
                    {<<".js">>, [<<"application/javascript">>]},
                    {<<".html">>, [<<"text/html">>]}
                ]}
            ]}
        ]}
    ],
    cowboy:start_listener(logtail_web_listener_http, 100,
        cowboy_tcp_transport, [{port, 9090}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    TailerSup = ?CHILD(logtail_tailer_sup, supervisor),
    Mgr       = ?CHILD(logtail_mgr, worker),
    
    Children = [ TailerSup, Mgr ],
    {ok, { {one_for_one, 5, 10}, Children} }.

