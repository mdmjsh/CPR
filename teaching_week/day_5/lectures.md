# OTP

## Generic Servers

- Abstraction of many reused client / server design patterns
- event servers
- finite state machines
- supervisors
- BOS (basic operating system)
    - This later become OTP

## OTP

- Abstract code into 2 parts:
    - specific code (business logic)
    - generic code
- system consists of set of loosely coupled applications
- leads to microservices architecture and monoliths
- leads to good hardware customisability
    - i.e. what system is required for what application
- 1 supervision tree per application
    - forces you to think in terms of supervision trees
- leads to 20x less code than c++ equivolent code
- only possible because of the underlying language semantics
    - links
    - monitors
    - exist signals
    - all highly optimised for soft-realtime

## Generic servers (more)

- General pattern define modules with callback functions to be invoked by the generic server.
- *Design by contract* - need to implement specific function names and return atoms in your modules - these are used for control flow.

- most important part of OTP
- allow encapsulation of a concept of a server
- generic behaviours are seperated out from rest of business logic
- handling process creation


### implemention

- implementing: add `-behaviour:gen_server`
- starting a server: `-gen_server:start_link(...)` - (instead of spawn)
    - gen_server:start_link/4 creates a new server
- `init/1` is called by the server callback module
    - initialises the process state and returns the loop data
    - this is user defined code in the Mod argument passed into gen_server.
    - n.b. the function has to be called init

- n.b. `init` is always the first function executed by the generic server
- basically OTP acts as a framework - we know as developers to start with the init func as the first point to check for issues etc.


## Message passing

### Syncronous
- `gen_server:call(Name | Pid, Message)`
- send a client a message
- `handle_call` gets exexcuted in the module
- `Receive` is no longer required
- tail recursion in the receive is now handled for you by the gen server


### asynchronous
-`gen_server:cast(Name | Pid, Message)` -> Ok
- `handle_cast` callback gets called in your module
- returns Ok imedidately

### Termination

- returning `stop` instead of reply / noreply atoms will lead to `terminate/2` function being called
    - true for `cast` / `call` if `stop` comes from init terminate isn't called (process just gets aborted(?)
- *design by contract* - has to return one of reply, no_reply or stop atoms - otherwise it'll cause an unknown exit.
- ANy messages which do not follow this protocol must get handled
- they are handled in the `handle_info/2` callback.


### Timeouts

- gen_server:call - will wait for 5 seconds and if no response it will terminate the client
- this is mechanism for avoiding deadlock
    - i.e. where clients are themselves gen_servers
- we can also put timeouts in the callback functions to override this behaviour

## Supervisors in OTP

- Recap: Erlang systems consist of supervision trees
- supervisors start and monitor children
    - through links and trapping exists

### Generic supervisors

Generic Behaviours:
- spawning the supervisor
- starting the children
- monitoring the children
- restarting the children
- stopping the supervisor
- cleaning up

Specific Behaviours:
- What children to start
- specific child handling
    - start restart
- child dependencies
- naming
- behaviours

- As with gen servers, split up code into generic code and biz logic with callback functions.

- Note children can be dynamic or static, e.g. we either know exactly the running child or we don't.

### Implementing

- `supervisor:start_line/3`
    - name, module, args (as per gen server).
- calls the `init` function in the module
- `init(args)` -> {ok, SupervisoSpec}

### Supervisor specs:

- another design by contract feature - returns tupe of maps relating to properties of the supervisor
    - e.g. restart straegy (one_for_one, one_for_all, rest_for_one) ... etc.


- also check out gen state M - generic finite state machine


CW

- Storage:
 - need a way to store state for the shopping cart - n.b. in memory on the proccess isn't enough as the process can go now
 - db.erl from class is fine
 - ets table in supervisor is an approach, what happens if the supervisor goes down?
 - do we need persistent storage
 - no correct answer here, but reason about the tradeoff and your approach.

- theoretical part 3. 'net split' means what if the network connecting two nodes become unavailable but the nodes themselves are still up.

therorical part - 'don't show me what I already know - show you understand the question, why it is relevent etc.'
    - e.g. don't explain how it works.

- submit code in zip file
- contain instructions on how to run it

francesco@erlang-solutions.com