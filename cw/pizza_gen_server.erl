-module(pizza_gen_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([synchronous_call/0, asynchronous_cast/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, pizza_server).

% start_link calls function gen_server:start_link/4.
% This function spawns and links to a new process, a gen_server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

% The synchronous request is implemented using gen_server:call/2
synchronous_call() ->
    gen_server:call(?SERVER, alloc).

% The asynchronous request implemented using gen_server:cast/2:
asynchronous_cast(Ch) ->
    gen_server:cast(?SERVER, {free, Ch}).

% If name registration succeeds,
% the new gen_server process calls the callback function
init(_Args) ->
    {ok, pizza:new()}.

% The gen_server calls handle_call(Request, From, State),
% which is expected to return a tuple {reply,Reply,State1}
handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = pizza:alloc(Chs),
    {reply, Ch, Chs2}.

% The gen_server calls handle_cast(Request, State),
% which is expected to return a tuple {noreply,State1}
handle_cast({free, Ch}, Chs) ->
    Chs2 = pizza:free(Ch, Chs),
    {noreply, Chs2}.