-module(transactional_db).
-export([start/0, stop/1, write/3, delete/2, read/2, match/2, loop/1]).

start() ->
    Pid = spawn(?MODULE, loop, [db:new()]),
    register(server, Pid),
    ok .

stop(Pid) -> unregister(Pid).

write(Server, Key, Element) ->
    Server ! {write, self(), Key, Element},
    ok .

delete(Server, Key) ->
    Server ! {delete, self(), Key},
    ok .

read(Server, Key) ->
    Server ! {read, self(), Key},
    receive
        Result -> Result
    end .

lock(Server) ->
    Server ! {lock, self()},
    ok .

unlock(Server) ->
    Server ! {unlock, self()},
    ok .

match(Server, Element)->
    Server ! {match, self(), Element},
    receive
        Result -> Result
    end .

loop(DB) ->
    receive
        {write, _Pid, Key, Element} ->
        NewDB = db:write(Key, Element, DB),
        loop(NewDB);

        {read, Pid, Key} ->
        Pid ! db:read(Key, DB),
        loop(DB);

        {delete, _Pid, Key, Element} ->
        loop(db:delete(Key, Element, DB));

        {match, Pid, Element} ->
        Pid ! db:match(Element, DB),
        loop(DB);

        {lock, Pid} ->
            loop_lock(DB, Pid)
    end .



loop_lock(DB, Pid) ->
    receive
        {write, Pid, Key, Element} ->
        NewDB = db:write(Key, Element, DB),
        loop(NewDB);

        {read, Pid, Key} ->
        Pid ! db:read(Key, DB),
        loop(DB);

        {delete, Pid, Key, Element} ->
        loop(db:delete(Key, Element, DB));

        {match, Pid, Element} ->
        Pid ! db:match(Element, DB),
        loop(DB);

        {unlock, Pid} ->
            loop(DB)
    end .
