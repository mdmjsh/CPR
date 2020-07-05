    -module(supervisor_).
-export([start/1, stop/1, loop/0, main/0]).


start(SupName, 5) ->
    print(SupName, 'Attempted to restart 5 times - removing process'),
    try unregister(SupName) of
        _ -> print(SupName, 'Unregistered')
    catch
        % How to catch badarg?
        error:Error -> print(SupName, 'Not in registry'),
    {error, max_retires}
    end;


start(SupName, Retries) ->
    print(self(), 'Starting'),
    start_child(self(), ?MODULE, 'loop', []),

    receive
        {ok, Pid} ->
            register(child, Pid),
            print(self(), 'Registered');

        {error, _} ->
            print(self(), 'Retrying'),
            start(child,  Retries +1)
    end,
    {ok, self()}.

start(SupNam) -> start(SupNam,  1).


start_child(SupPid, Mod, Func, Args)->
    Pid = spawn_link(Mod, Func, Args),
    print(Pid, 'Started'),
    SupPid ! {error, Pid},
    % SupPid ! {ok, Pid},
    {ok, Pid}.


loop() ->
    receive
        _ ->
            ok
        end.

print(Pid, Verb) ->  io:format("~w: ~w ~n", [Pid, Verb]), ok.

stop(SupName) ->
    exit(SupName, 'killed'), ok.

main() ->
    {_, Pid} = supervisor_:start(supervisor_proc),
    register(supervisor_proc, Pid),
    timer:sleep(2000),
    supervisor_:stop(Pid).