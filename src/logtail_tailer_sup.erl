-module(logtail_tailer_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{logtail_tail_file, {logtail_tail_file, start_link, []},
            temporary, brutal_kill, worker, [logtail_tail_file]}]}}.
