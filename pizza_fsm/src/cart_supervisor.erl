-module(cart_supervisor).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-export([start_child/2, stop_child/1]).
-export([init/1]).

% https://erlang.org/doc/design_principles/sup_princ.html
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [supervisor, []]).

init([Id, _Args]) ->
    storage:initDB([node()]),
    SupervisorFlags = #{strategy => simple_one_for_one,
                        intensity => 10,
                        period => 60},
    ChildSpec = [#{id => Id,
                    start => {cart, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [cart]}],
    io:format("Initializing supervisor with ~p ~n", [ChildSpec]),

    {ok, {SupervisorFlags, ChildSpec}}.


start_child(Id, {Mod, Fun, Argos}) ->
    io:format("start_child Id: ~p,  {~p, ~p, ~p}... ~n",[Id, Mod, Fun, Argos]),
    % N.b. this is currently failing
    supervisor:start_child(?MODULE, [Id, {Mod, Fun, Argos}]),
    % io:format("~p ~n", [S]),
    ok  .


stop_child(Id) ->
    supervisor:terminate_child(?MODULE, Id),
    ok.

stop() -> exit(whereis(?MODULE), shutdown).
