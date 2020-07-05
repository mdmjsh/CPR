
# Staff
- Francesco Cesarini
- Manuel-Rubio (TA)
- Robert Verding - co-inventor of erlang (Guest lecturer)


# Session 1

## Why Erlang?

- Erricson AXE phone switch - digital switch
- very difficult to maintain
    - patches in assembly!
- very successful commercially, but very high support/maintaince cost
    - developer productivity low
- Erricson founded a computer science lab in the mid 80s to devise programming next generation telecomms switches


### Telecomm problem domain
- massively concurrent
- soft-realtime
- peak loads
- distibuted by nature
- continuous operation
- quality and relability requirements

- The result of trying to solve these problems was the creation of Erlang and OTP.


## Erlang & the BEAM

- scales to millions of processess per VM
- No stop the world GC (oppossed to the stop the world GC of JVM)
- no shared memory - allow GC per processess
- Language semantics
    - distibution
    - error handling
    - concurrency
- Multi-core support in programming model
- VM introspection
    - tracing local function function calls
    - allows live tracing at scale
- Hot-code loading
    - software updates in life system
- Natively implemented function(NIFs) in C, Go & Rust - write your own C code and intergrate with erlang
- BIFs are built-ins - can't touch these, NIFs are your seam into erlang via other langs
- Erlang is turing complete, but a lot of the type conversions which are deferred to BIFs would be very inefficient to implement in native erlang


## Erlang Ecosystem
- ~30 langs running on Beam
- Erlang, Elixir ('erlang for hipsters')
- Elixir - combines elements of Ruby of Rails (Phoenix), Erlang, web-dev, closure to make something very powerful and familiar to a range of developers

## who's using erlang?
- cisco (erlang in 80% of devices)
- whatsapp backend (billions of messages per day, backend team 10 deverlopers(!))
- ericsson
- t-mobile
- nokia
- vmware
- lots of banking / payments services
- gaming (riot games)

## what in a name?
- Named after Danish Mathematician, Agner Krarup Erlang, one of the founders of telecomms
- `concurrency before erlang, or how erlang got it's name.` talk on youtube
- https://www.youtube.com/watch?v=0ytatCDEGyc

# Immutability for scale and resilience in Erlang
- Rabbit, Wolf, Carrot patch problem
- Two ways to do concurrency
    - Mutable state
    - Immutable state
        - processes don't share memory - they counicate together through messages

## immutability

imutabile: `Y = X^2 -1` - introduce a new varible, don't update the existing variable.
mutability: `X = X^2 -1` - Upate a variable based on some operations

In erlang, the processes themselves are immutable - i.e. only the processess themselves can alter their own memory
- In Haskel immutability is via Monads (e.g. io)

Shared memory (e.g. threads), commonly implemented in Java.
- common approach is to make immutable states within threads - only threads can altern their own memory.
- Note, the VM itself does have shared state, locks etc - goal to absract these away from the developer


Immutability ->       Concurrency ->                    Distribution
                no shared memory concurrency model         Gives distribution by default, i.e. no shared memory so can be run on multiple systems


Multi-core -> Parallelism <-Distribution

Distribution - Scalability
             - Reliability
reliability - as data is distributed, it is available on mutliple systems, e.g. fault tolerence if any machine goes down.
CAP theorm.

Amdahl formula - program will be as fast as it's slowest component when parralised - slowest component is sequential code
- architect code to distibute to be sent to data - only possible with no shared memory


### Corrupt state with Mutability
- If a mutatble thread crashes, all processes need to be stopped/ restarted as any/all access corrupt processes
- Very hard to make work with connectivity - what about network connection problems
- conncurrency on single machine can be very fast though

### Corrupt state with imutability
- If a immutatble thread crashes, the state for just that process will be cleared - other processes can continue (they have their own version of the data)
- Locality - distribution works as all locations have own copy of data
- In the case of network connectivity issue, algorithms (e.g. CRDT) can be used to merge the data back to a consistent state
- Note on CRDT - enables for out of order executions, based on the assumption that operations are transitive.

- Leads to distribution
- some slow down of computation on a single computation (e.g. network latency)
- however, after a certain point speed up is achieved by throwing more hardware at the problem
- No need for locks required
- push computation to data

## Multi-core
- Taihulight
- Parrellalboard
- Pi QuadCore
- Shift to multi-core jardware is inevitible
- Writting and debugging parallelised Java, C++, python etc. is hard
- imutabily is key
- handling failure of core can be handled in the same way as handling the failure of a process.

### Distriubting processes in erlang
- Erlang has schedulers, 1 per process to send a process to each core
- Each process gets given a number of 'reductions' (e.g. operations) to operate, after which it is put to the back of the queue and the next process gets to excute
- i.e. 'fair scheduling'
- i.e. each process gets given a fair ammount of time to execute
- interesting effect on peak loads - system will behave in a consitent way, latency might go up per request slightly, but messages will be delivered. No cost for context switching
- e.g. 1 million users all want to know if a number is a prime - in node.js if a user sent in a massive number it would block all other users (overall latencies will go up) - doesn't apply in erlang
    - i.e. just that person would have a long response time
- Stop the world GC (Java) - this can cause increase in latency at point of GC, doesn't apply in erlang
- N.b there is some possibility for reductions to be uneven (e.g. an addition vs length of a very lost list)

- If producers are faster than consumers (e.g. long message queues) - this is where context switching, GC etc. do start to lend to increased latency
    - this can be handled by auto-scaling - e.g. in kubernetes, after a threshold is hit spin up more containers.
- built in libraries for checking message queue size: `jobs` and ``


## Erl - REPL

b() - display all variable bindings
e(N) - repeat express in query (N)
f() - forget bingings

[erts-11.0.2] [source] [64-bit]             [smp:16:16]                    [ds:16:16:10]                   [async-threads:1] [hipe] [dtrace]
version                 arch                available:utalised cores        dirty scheduler                 IO threads


## Types and constructs

- ints - no max int size, after an int reaches a certain size it becomes a bignum - the max size is constrained by the system (e.g. RAM)
    - main hit comes from memory utalisation, e.g more garbage collection
- floats - not efficiently implemented (dynamic type system, also historically telecomms didn't rely on floating point operations)
- atoms - constant literals - start with lowercase letter or encapsulated by ''
    - mainly used to aid increase readibility in code
- booleans - just atoms (true, false), but some operators will return booleans.
- put into an `atom table` - global table which is created at runtime,
- each time an atom is created it is added to the `atom table`
- tuples, mixed data types allowed, static number of elements, access by position
- lists are dynamically sized, access by H|T
- strings are lists in erlang
- 'Proper', 'well-formed' lists are recursively defined. However, non well-formed list are allow to enable lazy evaluation
    - e.g. [1,2 Fun] - where Fun is function that returns a list.

# Variables

- Once bound, are immutable
- Dynamic type system
- Types are determined at runtime
- Very very difficult to now add a static type system due to design choices in the language
- Alot of the complexity lie in the message passing implemention
    - i.e. `recieve` clauses can take any valid data type
- Bound in the scope of the function

# Pattern matching
- Pattern can contain bound/unbound variables
- expression cannot contain unbound variables

