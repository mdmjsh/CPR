# Process Design Patterns


- Giving processes particular behaviour

## Common Patterns
- Client server models
- finite state machines
- event managers
- supervisors

## Client server models: Process skeleton

"Process of processes"
- start -> initialise -> loop -> stop -> terminate
- You'll often see this pattern, and these names by convention cropping up again and again
- init
    - setup resources
- terminate
    - cleanup resources
- Processes can be used to implement client and server solutions
- client/ server comms handled via message passing
    - often hidden being functinoal interfaces
    - i.e. server could be on a different physical machine / different location / different node on the same machine
    - any valid erlang data type can be sent as a message
    - need to manually set up machines and their VMS to talk to each other
        - i.e. by configuration
        - autodiscovery of nodes is possible too

- `register/2` BIF - register a process, used for setting up servers.


## Finite State Machines

- not that commonly used, but very powerful design pattern
- Processess can be used to implement FSM
- FSM, all maps all of the states of a systems driven by events
    - e.g. Day, Night, driven by events sunset, sunrise.
- each state is a tail recursive function
- each event is an incomming message

- event event is a message
- every state transition is a TR function
- big difference between erlang and other concurrent langauges is the select recieves
    - this means you don't worry about messages coming out of order, they can just sit in the mailbox whilst
    the filter messqges are being dealt with
    - means you don't need to worry about dealing with extra complexity of dealing with handling messages in correct order, or handling invalid messages explicity
    - greatly reduces complexity of implementing the system.
    - harks back to origin in telecomms, which is essentially a very complex FSM system.


## Example - Mutex
 `Mutex` is a program that allows multiple processes to share the same resource
- states - busy, free
- events - wait, signal

```erlang
-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])).
stop() ->
    mutex ! stop.
init() ->
    free().

wait() ->
    call(wait).
signal() ->
    call(signal).
%% We hide all message passing and the message protocol in
%% functional interfaces.
call(Message) ->
    mutex ! {Message, self()},
    receive
       {reply, Reply} -> Reply
    end.
reply(Pid, Message) ->
    Pid ! {reply, Message}.

%% The state functions.
free() ->
    receive
        {wait, Pid} ->
            reply(Pid, ok),
            busy(Pid)
    end.
busy(Pid) ->
    receive
        {signal, Pid} ->
            reply(Pid, ok),
free() end.

```

## Event Managers and Handlers
- Processes can be used to implement event manager
- typical events:
    - alarms
    - state changes
    - commands
    - errors
- when an event is recieved, one or more operations are applied to the event
- operations can be switched on/off depending on environment

## Supervisors

- Processes whose only task is to start, monitor and manage children
- child processes are either:
    - workers
    - supervisora
- supervisors can restart the children when they terminate
    - i.e. non-defensive programming
- build a supervison tree
- n.b. Go doesn't implement this type of supervision, so you're forced to use defensive programming
- no business logic in supervisors, just configuration
- e.g. supervisors should have very, very low chance of crashing
- let it crash in child, but not in supervisor.

"we can just dig holes with our bare hands... or we can use a shovel" Mathew Daiter on using lists vs. Recurssion

- Joe Armstrong interviews Alan Kay (check out interview)

## Process error handling

### Links

- `link/1` creates a bi-directional link between two Pids
- if a process terminates abnormally, it sends an Exit signal to all linked Pids
- This then propegates to all linked processed (i.e. a propagation path)
    A -> B -> C
- Processes can 'trap' exist signals by calling `process_flag(trap_exit, true)`
- The signal will be converted to a message and placed in the proccess' own mailbox.
- Pid B will deal with the exit message, but not propagate the signal to Pid C.
- Generally processess are only linked to a supervisor, long links are avoided to avoid a propogation of crashing the system.

## progation semantics

_ when a process terminates it sends an exit signal to linked processes and terminates
- Exists can be norm or abnormal

## monitors


PidA -> PidB
`erlang:monitor/2` will process calling the BIF monitor the process PidB. Monitoring is uni-directoinal and stackable.
- When process Pidb dies then PidA will be sent the message, saying 'I'm down'
- `demonitor/1` - this will stop monitoring the monitored process
    -  should also be called with the [flush] flag
    `demonitor(Ref, [flush])`
    - otherwise, you have a race condition whereby a process could send the down signal after the process was demonitored
        - causing either the message to sit a mailbox forever
        - or get sent to a mailbox that is not longer monitoring it
- monitoring is only available for processes on the same machine (not distibuted)

## Robust systems

- Robust systems can be designed by layering workers and supervisors
- This can be achieved by finding dependencies between processes
- Seperate out independent systems and enforce through supervision trees


## Synch Vs Aynch

- When you're returning 'ok' from a message being sent, always consider whether it needs to be asycn / sync
- i.e. do you need to wait for the result of the call, or can you just continue?
- Consideration - queue size (ie. in mailbox), you don't want losts of requests queueing waiting for synch returns