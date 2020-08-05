-module(cart).
-behaviour(gen_statem).
-export([start_link/1]).
-export([init/1, callback_mode/0]).

-define(FSM, cart).

%% Public API
%% ------------------------
start_link(UserName) ->
    ReferenceId = create_reference(UserName),
    gen_statem:start_link({local, ?FSM}, ?MODULE, ReferenceId, []),
    {ok, ReferenceId}.


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
    io:format("starting cart...~n"),
    storage:add_item(ReferenceId) .

%% Private API
%% ------------------------
add(ReferenceId, Item) ->
    gen_statem:cast(?FSM, {add, ReferenceId, Item}).


remove(ReferenceId, Item) ->
    gen_statem:call(?FSM, {remove, ReferenceId, Item}).

stop() ->
    gen_statem:stop(?FSM).

create_reference(_) ->  uuid:to_string(uuid:uuid4()) .


%% State functions
%% -----------------------

shopping(call, {add, ReferenceId, Item}) ->
    Item, ItemPrice = storage:lookup(Item),
    CurrentBasket = storage:get_cart(ReferenceId),
    Items = CurrentBasket.Items | Item - add the item to the basket
    % Total = CurrentBasket.Price  + ItemPrice
    storage:add_item(ReferenceId, Item, Total),
    {ok, Items, Price} .

shopping(call, {remove, ReferenceId, Item}) ->
    Item, ItemPrice = storage:lookup(Item),
    CurrentBasket = storage:get_cart(ReferenceId),
    % Items = lists:delete(Item, CurrentBasket).
    % Total = CurrentBasket.Price  - ItemPrice
    storage:add_item(ReferenceId, Item, Total),
    {ok, Items, Price} .


