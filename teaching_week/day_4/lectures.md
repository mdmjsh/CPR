
# Maps and records

## Records
- Records - class equivolent
- access records by name
- items can be easily add/removed by name reference
- N.b. single assignment still applies.


### Defining a record
- `record(person, {name, age=0, phone})`
- record definitions are stored in header files - `.hrl` files
- `-include(blah.rhl)`
- enables sharing definitions across modules

### Instanitating a record

- `Person = #person{name = "joe", phone=[1,2,3,4]}` - n.b.age would be set the default value 0
- 'Person' is now bound as a variable to the record above
- access: Person.age
- in theory you can replicate classes in OO,
    - `record(person, {name, age=0, phone, sing=sing/0})` - assign an attribute to a function
- Persion.sing()
- N.b. has to be single arity as variables have to already been defined within the function
    - I.e couldn't be defined in header files, as the variables are not yet bound
- Not sure if that's correct syntax, but something like that


### Patten matching
- Bound records can be used to pattern match on variables and function heads/guards
- guard: `when is_records(P, person)`
- in function head: fun(P = #Person{name="Joe"}) -> ...
- n.b in function head is the prefered approach - slightly faster
- n.b. `=:=` is the prefered equality check in a guard because of compiler optimisations
    - e.g. it will only attempt to match if the datatype is correct.

### Records: internal represenation

- Records are repesented as tuples by the run time system
- The precompiler translates the creating, updating and selecting operations on records to operations on tuples
- N fileds in teh record will result in a tuple with N+1 elements
- The first elemeent is the name of the record
- WARNING - never use the tuple repreesentation of records!
- `#person{} =:= {person, undeefined, 0, undefined` - n.b. undefined is an atom here.


### Records: info
- `record_ino(fields, RecType)`
- more helpers avaialbe

### Records in the shell
- `rr(person)`
- `rl`
- `rd()` - define a record in the shell
- `rf/0` `rf/1` - forget a record


## Maps

- Richard o'keife - introduced maps to erlang
- analogous to dictionary
- not as fast as records
- defined: `Map = #{Key1 => Exprssion1, KeyN => exprN}`
- init: `Person - #{name => 'Joe, phone => [1,2,3,4]}`

### field selectors
- `maps:get(name, P)` - will raise a runtime exception if trying to get a value with hasn't been set or a key which doesn't exist in the map.
- Can use pattern matching as well.
    - will match any maps with the matching elements
    - e.g. `P = #{age=25}` will match any map with age=25, regardless of other keys in the maps

### Updating
- `=>` updates existing fields or adds new fields
- ` :=` only changes existing fields
- Again, single assignment and immutability rules apply


### Records vs Maps
- Records:
    - static and compile time checking
    - faster
    - more difficult to modify

- Maps
    - Dynamic
    - Easier to modify
    - Easier with large number of keys


## Erlang Term Storage (ETS)

- Only mutable datastucutre in erlang
- Mechanism to store large data quantities
    - data is stored as tuples
- Data is stored in dynamics tables and accessed through keys as hash tables or binary trees
- O(1) lookup time regardless of table size
- low level search mechanism - v. efficient
- no transaction handling

- Hash(key) points to a memory position
    - a function function maps the key to values position in memory
    - i.e. uses constant space

```
% Public access table, dennoted by empty list
6> TabId = ets:new(myTable, []).
#Ref<0.2843365945.3036020746.217453>

# Private table, the atoms in the list control the settings of the table
7> TabId2 = ets:new(myOtherTable, [named_table, private, bag]).
myOtherTable

```
## table options:

- `bag` - key doesn't have to be unique, but tuple does
- `set` - key will be a key (i.e. unqiue) - uses set logic
- `duplicate_bag` - non unique tuples (k,v )
- `ordered_set` - unqique keys, traveresed linerly
- `named_table` - allows access to the table by its name, not just reference

- access rights: public,  protected, private

insert:
```
8> ets:new(countries, [set, named_table]).
countries
9> ets:insert(countries, {luigi, italy}).`
```

Lookup:
```
3> ets:lookup(countries, luigi).
[{luigi,italy}]
```

- all ets operations are BIFS, and each has 1 reduction
- i.e. this guarentees atomicity - no risk of process failing with table in inconsistent state


### traversing tables

```
K1 = ets:first(countries)
ets:next(countries, K1)
```


### Pattern matching

- can be used a select to the ETS
- `ets:match(countries, {'$1', ireland, '_'})` - return second element of anyone who's country is ireland
- N.b. this is 0 indexed - rest of language is 1 indexed!
- `ets:match_object(tableID | TableName, Pattern)`
- `ets:match_delete(tableID | TableName, Pattern)`

## match specification
- consists of an erlang terms, very hard to read
- `ets:select()`
- `ets:fun2ms(LiteralFunc)` - translates a literal function into a match specification
    - handled at compile time
    - no more expensive, far greater readability
    - the `LiteralFun` must be a typed (i.e. annomynous) in function - can't be a reference to a defined function

## Issues

- Tables are linked to the processes that created them
    - Bind to a supervisor and give public access if you need some persistence/recoverability
- if the process dies the table dies
- match operations are implemented as BIFs
    - Match operations on big tables stop other processes from executing
    - first/next preferable to match as won't disrupt other processes in the system.
> "If the key is specified in the pattern, the match is very efficient. If the key is not specified, that is, if it is a variable or an underscore, the entire table must be searched. The search time can be substantial if the table is very large."
(https://erlang.org/doc/man/ets.html#match-1)

- Tables are not garbage collected
    - must be deleted manually
- All stored in RAM, very fast lookups
    - 'redis of the erlang world'
- with >20 elemerbs, ETS tables are more efficient with lists
- `dets` - Disk only ETS module
- there is a max no of ETS tables allowed - this is configuratble

### Info
```
22> ets:i(countries).
<1   > {bob,uk,london}
<2   > {luigi,italy}
EOT  (q)uit (p)Digits (k)ill /Regexp -->q
ok
```
### Schemaless

- nothing preventing you adding rows with inconsistent schemas:

```
ets:new(countries, [set, named_table]) ,
ets:insert(countries, {bob, italy, florence}),
ets:insert(countries, {luigi, italy}).
% cause a match error - schemas in rows are inconsisten
MS = ets:fun2ms(fun({Name, Country, City}) when Country == italy -> [Country, Name] end).
```


# Distributed Programming

- recap:
    - no shared memory
    - message passing
- very similar to SmallTalk
- Alan Kay (SmallTalk) - 'the real point of OO was message passing, all this inheritance stuff came much later'

- Messages can be sent to different cores, chip sets, or different psyhical machines
- in all cases `Pid ! msg`, i.e. agnostic of destination
- loosely coupled processors
    - processors can come and go, the program keeps on running

- Distribution gives:
    - scalability (horizontal scaling)
    - reliability - any system can crash

- start multiple nodes `erl -sname bar`
- `net_adm:ping('bar@machine_name') .` - connects the two processes
- you can now send messages back and forward
- in bar: `receive P -> P end.`
- from foo: `P ! hi`
- from bar: flush()
- ...etc.

less ~/./.erlang.cookie

- this file is to communicate between the nodes
- nodes can only communicate is there know the cookie of the node they are trying to communicate with.

- Nodes are loosely connected to each other
    - connections are based on TCP/IP
- nodes can come and go dynamically in a similar manner as process and containers
    - e.g. spun up and down at will.
- `Net Kernel` is the process that coordinates operations in a ditributed node


## Deployment

- Docker isn't a good candiate for Erlang, because of it's layered defence approach.
- without docker (bare metal):
VM
    Application
        Supervisor
            Process
                Beam

- If anything goes wrong you can escalate restarts up the stack, all the way to the hardware

with docker:

Container
    OS
        VM
            Application
                Supervisor
                    Process
                        Beam

- Because of the immutability of containers if anything goes wrong you have to destroy the container and then relaunch.
- The more complexity you add, the more liekly it is for a process to fail or something to go wrong elsewhere (e.g. security).

- The fallacies of distributed computing:
    1. the network is reliable
    2. latency is zero
    3. bandwidth is infinite
    4. the network is secure
    5. topology doesn't change
    6. there is one administrator
    7. transport cost is zero
    8. the network is homogeneous



## message sending strategies

1. at least once
2. at monce once with notification
3. at most once (aynch)


- baring in mind that there are several things that can go wrong when sending distributed messages
- don't differentiate between error types in the code, just send a message and either send or fail.
    - greatly simplies the approach