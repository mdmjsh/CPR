-module(sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
start_link() ->
    supervisor:start_link({local, ?MODULE},?MODULE,  []).
init(_) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 2, period => 3600},
           [#{id => test, start => {test, start_link, []},
              restart => permanent, shutdown => 2000,
              type => worker, modules => [test]}]}}.