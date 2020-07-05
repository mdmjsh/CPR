-module(thering).
-export([start/2, init/2]).

start(ProcNum, Count) ->
    spawn(?MODULE, init, [ProcNum, Count]).

init(ProcNum, Count) ->
    io:format("Leader started: ~p~n", [self()]),
    % ProcNum-1 = Pid of leader
    Pid = spawn(?MODULE, start_proc, [ProcNum-1, self()]),
    send_message(Count, Pid)
    loop(Pid),
    io:format("Leader sopping~n").


start_proc(1, Leader) ->
    io:format("~w:~w started~n", [self()]);
start_proc(ProcNum, Leader) ->
    io:format("~w:~w started~n", [ProcNum, self()]),
    Pid = spawn(?MODULE, start_proc, [ProcNum-1, self()]),
    Pid ! ok,
    receive ok -> ok end,
    io:format("~w:~w stopping~n", [ProcNum, self()]).

send_message(0, Pid) ->
    ok;
send_message(Count, Pid) ->
    Pid ! ok,
    receive ok -> ok end,
    send_message(Count-1, Pid).

loop(Pid) ->
    receive ok -> ok,
    io:format("~w go ok~n", [self()]),
    Pid ! ok
    end,
    loop(Pid).


% The Process Ring
% Write a program that will create N processes connected in a ring. These processes will then send M number of messages around the ring and then terminate gracefully when they receive a quit message.
% Hint:
% There are two basic strategies to tackling your problem. The first one is to have a central process that sets up the ring and initiates the message sending. The second strategy consists of the new process spawning the next process in the ring. With this strategy you have to find a method to connect the first process to the last.