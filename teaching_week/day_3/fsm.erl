-module(fsm).
-export([start/0, enter/1, insert_coin/2, fsm_loop/2, main/0]).



fsm_loop(State, Sender) ->
    receive
        {start} ->
            fsm_loop(closed, Sender),
        ok;
        {enter} ->
            fsm_loop(closed, Sender);
        {insert_coin, Coin} ->         print(self(), print(Coin, 'received')),

        case State == closed of
            true -> Sender ! print(self(), 'Opening'), fsm_loop(open, Sender), ok;
            false -> Sender ! print(self(), 'Opening'), {error, access_denied}
        end
    end .


start() ->
    print(self(), 'starting fms_loop'),
    Pid = spawn(?MODULE, fsm_loop, [closed, self()]),
    print(Pid, 'Started - Waiting for a coin...'),
    {ok, Pid}.

enter(Pid) ->
    print(Pid, 'Entering'),
    Pid ! enter .

insert_coin(Pid, Coin) ->
    print(Pid, 'Inserting Coin'),
    Pid ! {insert_coin, Coin}.

print(Pid, Verb) ->  io:format("~w: ~w ~n", [Pid, Verb]), ok.


main() ->
    Pid = fsm:start(),
    fsm:insert_coin(Pid, coin),
    fsm:enter(Pid).
