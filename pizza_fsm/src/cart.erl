-module(cart).
-behaviour(gen_statem).
-export([start_link/1, add/2, remove/2, shopping/3]).
-export([init/1, callback_mode/0]).

-define(NAME, cart).

%% Public API
%% ------------------------
start_link(UserName) ->
    ReferenceId = create_reference(UserName),
    gen_statem:start_link({local, ?NAME}, ?MODULE, ReferenceId, []),
    {ok, ReferenceId}.

add(ReferenceId, Item) ->
    io:format("preparing to add ~p to cart ~p... ~n",[Item, ReferenceId]),
    {Type, Item_} = Item,
    Id = atom_to_list(Type) ++ atom_to_list(Item_),
    gen_statem:cast(?NAME, {add, ReferenceId, Id}).

remove(ReferenceId, Item) ->
    io:format("preparing to remove ~p   ~n",[Item]),
    {Type, Item_} = Item,
    Id = atom_to_list(Type) ++ atom_to_list(Item_),
    gen_statem:cast(?NAME, {remove, ReferenceId, Id}).

%% Callback implementations
%% ------------------------
callback_mode() ->
    % http://erlang.org/documentation/doc-10.3/doc/design_principles/statem.html#State%20Enter%20Calls
    % You return a list containing state_enter from your callback_mode/0
    % function and the gen_statem engine will call your state callback once
    % with the arguments (enter, OldState, ...) whenever the state changes.
    % Then you just need to handle these event-like calls in all states.
    [state_functions, state_enter].

init(ReferenceId) ->
    io:format("Starting cart... ~n"),
    {ok, shopping, ReferenceId} .

%% Private API
%% ------------------------

stop() ->
    gen_statem:stop(?NAME).

create_reference(_) ->  uuid:to_string(uuid:uuid4()) .


%% State functions
%% -----------------------

shopping(enter, _, ReferenceId) ->
    io:format("Shopping cart ~p initialized  ~n",[ReferenceId]),
    storage:add_item(ReferenceId, init),
    {keep_state, {}};

shopping(cast, {add, ReferenceId, Item}, {})  ->
    io:format("Adding ~p to cart  ~p~n",[Item, ReferenceId]),
    storage:add_item(ReferenceId, Item),
    {keep_state, {}};

shopping(cast, {remove, ReferenceId, Item}, {}) ->
    io:format("Removing ~p Item from cart  ~p~n", [Item, ReferenceId]),
    storage:remove_item(ReferenceId, Item),
    {keep_state, {}} .


