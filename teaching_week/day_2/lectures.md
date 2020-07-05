# Sequential Erlang

## Conditional Evaluation

### case

```erlang
    case lists:member(foo, List) of
        true ->;
        false -> {error, unknown}
    end
```

- One branch should always suceed, otherwise it will terminate with an error at runtime.
- Can leave `_` var as a catch all
- case statment is itself an expression, e.g. you can assign the output to a varible
    - e.g. `X = case ...`
- Don't use case statements for Defensive Programming, better to let it fail and use erlang error handling to detect crash and recover.
* Error Handling *
    - Isolation error in process
    - Then create an escaltion process to recover / restart processes
    (More on this later)

### Gaurds
- Keyword `when`
- Fully guarded clauses can be re-ordered
- guards can be used in: (function heads, case clauses, receive and if expressions)
- gaurds can be used to type check in function heads:
    - `number(Num) when is_integer(Num) ...`
- joining guards `,` (and) `;` or
- all guards have to be free of side effects (i.e. pure) - for this reason, you're not allowed to use user defined functions as a guard

### If

- one branch must always succeed
- Using true guard, we ensure that a guard always matches
- first match is evaluated
- as in guards, user defined functions are not allowed
- N.b. the case constuct allow user defined functions as it treats the function as an expression

## Recursion

### Traversing lists

- break list into head and tail, use the recursive definition of a list.
- N.b. only works on proper well form-lists, so be careful of non-wellformed lists.
- Leads to self-describing code:
    e.g. sum of empty list = 0
    sum of non-empty list = H + sum(tail)

Recursion walkthrough example
```erlang
len([_|T]) -> 1 + len(T);
sum([]) -> 0.
```

len([1,2,3]) - 1, T = [2,3]
    1 + (len([2,3]))
        2 + len([3])
          3 + len([])


# More recursive patterns

- Append head to a new list, recurse on tail
- Very similar to how `map` works
`double([H|T]) -> [2*H | double(T)]`

- Using predicates

`memeber/2` is  a predicate looking for an element in a list.
```
member(H, [H|_]) -> true;
member(H, [_|T] -> member(H,t);
member(_, []) -> false.

```

### Tail Recursion

- Last clause is a recursion
- Non-tail recursive is not automatically compiled to TR - programmer needs to handle this themselves
- tail recursion is faster and takes up less stack space.

## Sequential Error Handling

- 'onion model' - various layers of defence


### Runtime Error: match
- see /chpt3/chapter_3.md for taxonomy of errors.
- Types of exceptions: `Error`, `Exit`, `Throw`
- Catching exception - Try...Catch
- You can pattenr match on the error or error type to catch specific errors.

### Throws
- Non-local reutnr in deep recursive function calls
- Execution flow jumps to the first catch in the execution stack



## Concurrency

- Phoenix - successor of Ruby on Rails, running on Beam and built in elixir.
- Achieved 2M websocket connections ('the Road to 2 Million Websocket Connections in Phoenix` - atricle)
- Each websocket is a process in the Beam.


### Concurrency vs Parallism
- Concurrency - group together processes
- run on different machine
- allows for system to scale
- Parallism will actually speed up your processes
- e.g. 10 cooks making pasta example
- Note, load can be IO bound or CPU bound
- Concurrency happens when your code is running in different processes
- concurrent solutions can exploit the underlying system's parallelism if present
- parallelism can speed up execution
- N.b. Pool managers can be used to make sure that you don't spawn an exponential number of processes
    - e.g. webcrawler, where every link found starts a new webcrawler process (Monzo interview question)

### Creating Processes

#### Terminology
Process := A conncurrent activity. The system may have may concurrent processes running at a time
Registered Processes := a process with an alias


- Call BIF `spawn` called with Module, Function [Args] - same as `apply`
- spawn(db, create, [1,2,3])
- This returns a pid which we can use to communicate with the process
- `Pid2 = spawn(M, F, [A])`
- process Id = x.x.x
            [machine]. [random id]. [node name]
- spawn never fails - only when max number of running processes is reached `erlang:system_info(process_limit)` - checks the current limit (default 264k, max: 167M)
- Make processes handing termination
    abnormally: runtime errors
    normally: no more code to execute

### Message Passing

- `Pid!Msg` expression for sending messages - Msg is any valid Erlang data type
- Messages sent to non-existing processes are thrown away
- Recieved messages are stored in the process' mailbox

```
4> self() ! hello.
hello
5> flush().
Shell got hello
ok
6> i().   % list of all processes
```

- N.b. Mailboxes are essentially FIFO queues, messages are kept in the order they are received in.
- Each time a message is read it is removed from the mailbox.
- sending message with `!` (bang) symbol just puts the message in the mailbox, it isn't read until `flush` is called.

### Receiving messages

- bookended with `receive`, `end` keywords
- FIFO recieving
- can use pattern matching
    - FIFO with a filter
- "selective recieve"
- `receive` supsends the process until a matching message is received
- a message doesn't have to match (opposed to case statments)
- recieves are asyncronous
- `recieve` clause would live inside a function
- unmatched messages stay in the mailbox - match messaged get processed
    - Need to be careful of DDoS - e.g. filling up mailboxes with unmatched patterns
    - or sending matching messages, but flooding the server.
- `receive` loops are lazily evaluated
- N.b. selective recieve is an event trigger
    - i.e. if a process is sleeping, on receiving a matching message it will awake and start processing again.
- receive is syntactically similar to `case ... of`

```
receive
    Pattern1 when Guard1 -> Expr1;
    Pattern2 when Guard2 -> Expr2;
    Pattern3 -> Expr3
end
```

### Message binding

- Pid is bound on the calling end
- any data resulting from a pattern matching is bound on the receiving end


## Registered Processes

- `register(Alias, Pid)`
- Registers the process PId with an Alias (has to be an atom)
- Any process can send a message to a registered process
- `registered/0` returns all registered process names
- BIF `whereis(Alias)` returns the Pid of the process with the name Alias.
- `Alias ! message` - send a message to an alias
- sending a message to non-existing registered processes causes the calling process to terminate with a `badarg` error.


```
go() ->
    Pid = spawn(echo, loop, [])
    register(echo, Pid).
% register the echo processes - now any process which knows it's alias can call the process
```

### Timeouts
```
recieve
    Msg ->
        exp
    after 1000 ->
        {error, timeout}
    end.
```
- Need to make sure that sending messages handle the time out response.
- i.e. this becomes a synchronous request - need to wait for the reply to come back
- can be seen as a bit of an antipattern


## Process skeleton

Process skeleton := start->initialise -> loop -> stop -> terminate
    - pattern of all patterns
    - all processes will tend to follow this pattern
- start the process calling some init with Args
- start a loop (receive, evaluate loop)
- Loop: PM on a given message and process
- stop: PM on a given message and stop the loop
- terminate the process
- cleanup

# Observer Processes
- Graphical toll used to processes
- `observer:start().`