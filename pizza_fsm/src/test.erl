-module(test).
-export([start_link/1, init/1]).
start_link(Name) ->
    Pid = spawn_link(test, init, [Name]),
    register(Name, Pid),
    {ok, Pid}.
init(Name) ->
    io:format("~p started~n", [Name]),
    loop().
loop() -> receive X -> X end.