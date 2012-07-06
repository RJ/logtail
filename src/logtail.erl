-module(logtail).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

start() -> 
    lager:start(),
    application:start(gproc),
    application:start(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    logtail_sup:start_link().

stop(_State) ->
    ok.
