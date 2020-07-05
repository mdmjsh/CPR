-module(gen_server_db).
-export([start/0, stop/0, read/1, write/2, match/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).
% define the db atom as a constant - good practice
-define(SERVER, db).

start() ->
    gen_server:start_link({local, ?MODULE}, db_server, [], []).

stop() ->
   gen_server:stop(?SERVER).

init([]) -> {ok, db:new()}.

write(Key, Element) ->
    gen_server:cast(?SERVER, {write, Key, Element}).

read(Key) ->
    gen_server:call(?SERVER, {read, Key}).

match(Element) ->
    gen_server:call(?SERVER, {match, Element}).

delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).


handle_cast({write, Key, Element}, DB) ->
    Db2 = db:write(key, Element),
    {noreply, Db2}.

handle_call({read, Key}, _From, DB) ->
    Reply = db:read(Key),
    {reply, Reply, DB};

handle_call({match, Key, Element}, _From, DB) ->
    Reply = db:write(Key, Element),
    {reply, Reply, DB};

handle_call({match, Element}, _From, DB) ->
    Reply = db:delete(Element),
    {reply, Reply, DB}.

handle_info(_Info, DB) -> {noreply, DB}.

terminate(Reason, DB) ->
    io:format("terminating with reason ~p~n", [Reason]),
    ok.
