# Robert Virding - co-creator of erlang

- Joe armstrong and Mike Williams are the others


# The Erlang Rationale

`Rationale` := exposition of principles or reasons
- understanding rationale can help users become better users
- understanding of the problem evolved at the same time as the language evolved
    - i.e. user feedback, lean
- weren't trying to implement a functional language
- weren't trying to implement actor model
    - both of these happened along the way - the point was trying to solve the problem, e.g. solution driven design.
    - very pragmatic development

## First Principles

- Lightweight concurreny
    - large number of process
    - process creation, context switch, inter-process comms must be cheap and fast
- Asynchronous communication
    - much easier to build synchronous communication on top once asynch is in place
- Process isolation
- Error handling
- Continuous evolution

Also:

- high level language to get benefits
- simple in the sense of small number of basic principles, allowing for extensibility, rather than providing a language rich with small features (ala ruby)

- Sequential Language
    - simple functional language
    - no global
    - no shared
    - immutable
    - pattern matching
    - dynamically typed
    - no user defined data-types
Concurrency
    - millions of processes of a machine (max 135m on Beam in one machine)
    0 processess are used for everything
        - concurrency
        - managing state
    - processess are isolated
    - aysnchronous communication
    - no global data


# Erlang things

- Immutable data strures
- Processes
    - everything with internal state
    - processes manage state
    - data inside a process is still immutatble
- ETS table, is a mutable data structure, but the data within it is immutable

## Processess

- parallel indepedent execution
- communications through asynch message passing
- links/montiros for error handling
- obey/transmit exit signals

- Everything is run within a process
- All processes are equal - no special or system processes
- No process hierarchy - flat process space

# Process Communication

- via messages
- all process communication is asynch.

# Process BIFs asyncronous
    -only checks arguments

- everything being aysnch is it works very well with distribution.

# Error handling

- system must never go down!
- parts may crash and burn, but the system must never go down
- System must be able to:
    - detect
    - contain
    - handle
    - recover

- Don't want to be checking for errors in the code everywhere though
- Desiderata - handle process crashes amongst linked processes
    - If one process crashses then all cooperating processes should crash
    - cooperating are linked together
    - Process crashes propagate along link

# OTP (Open Telecoms Platform)

- Erlang just a language, OTP is a set of large standard libraries
- tools, patterns, generic behaviours, extensibility
- Application, Supervisors and Workers

# Systems

- Systems built with Erlang tend to be very OS like
- very seldom a central thread of execution
    - At most something which starts seperate tasks
- 'Not so much a lnag with concurrency, rather a system with a language within it'

## IO

- IO is proccess based
- Built around I/O servers
- The interface between I/O requests from Erlang and the I/O device
- Process groups, group leaders

## Process groups, Jobs

- each group has a group leader, that's it

## records
- Added to solve the problem of:
    - Named fields in tuples
    - same efficiency as normal tuples

# Problem domain
- realised that the problem domain was wider than just telecoms
- conncurrency and fault torelence
- Concurrency model scales very well


# Scheduling Dirty NIFs

- clean schedulers run erlang code
    - literally just count function calls
    - after N function calls, scehdule out the rest
    - Means no erlang process can block the system
        - after N calls the rest gets scheduled out
- NIFS
    - e.g. native C code
    - hands over control
    - in theory could take control of the system and never pass control back to erlang system
    - these are marked as 'dirty' and a dirty scheduler is created
        - this creates native OS system thread - the OS takes care of load balancing etc.
    - It's up to the OS to load balance these, it may cause the system to slow down overal, but the system will handle it.

- The Beam is were all the magic happens of managing scheduling, load balancing etc.
- the Beam can almost be seen as an OS - all of the above can be seen as OS responsibilities, beyond just making the code run.

# Erlang as a data processing framework
- disco project (nokia research) - 2008, 2011 was gaining traction but closed after Nokia shutdown research lab following 2007 financial crash
-  Also CouchDB, implemented in erlang - cloud based processing, now an Apache project since 2017.
