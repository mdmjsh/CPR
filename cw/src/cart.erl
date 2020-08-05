-module(cart).
-behaviour(gen_statem).
-export([start_link/1]).
-export([init/1,callback_mode/0]).

-define(FSM, cart).

callback_mode() ->
    state_functions.


start_link(UserName) ->
    ReferenceId = create_reference(UserName),
    gen_statem:start_link({local, ?FSM}, ?MODULE, ReferenceId, []),
    {ok, ReferenceId}.


stop() ->
    gen_statem:stop(?FSM).

% ReferenceId is a unique reference associated with the user and their shopping card. Note that you should ​not​ use a Pid.
% Depending on your implementation, you might need a name server storing the mapping between the reference and the Pid.
create_reference(_) -> {reference_id} .

init(ReferenceId) ->
    %  Inits a new ETS table with a ReferenceID for the user
    Cart = ets:new(cart, [named_table, ordered_set]),
    ets:insert(cart, {ReferenceId, []}),
    {ok, shopping, Cart} .

% State Transitions

shopping(call, {add, ReferenceId, Item}) ->
    ets:insert(baskets, {ReferenceId, [Item]});

shopping(call, {remove, ReferenceId, Item}) ->
    ets:insert(baskets, {ReferenceId, [Item]}).

shopping({call, ReferenceId}, Data) ->
    {next_state, payment, Data, [{reply, ReferenceId, payment}]};


shopping({call, ReferenceId}, Item) ->
    ets:insert(baskets, {ReferenceId, [Item]}),
    {keep_state, Data#{balance:=NewBalance}, [{reply, From, deposit_made}]};



payment() -> {} .
delivery() -> {} .








% API Functions

add(ReferenceId, Item) ->
    gen_statem:call(?FSM, {add, ReferenceId, Item}).


remove(ReferenceId, Item) ->
    gen_statem:call(?FSM, {remove, ReferenceId, Item}).

add_item(ReferenceID, Item) ->
    [Old] = ets:lookup(cart, ReferenceID),
    New = [Old | [Item]],
    Success = (1 =:= ets:select_replace(cart, [{Old, [], [{const, New}]}])).



% % These functions will add and remove a single item from the shopping cart.
% % The item needs to match the item as described in the ​menu.txt​ file.
% add(ReferenceId, Item) -> ok | {error, unknown}.
% remove(ReferenceId, Item) -> ok | {error, unknown}.

% % Transitions the FSM to a new state,
% % where payment details and delivery address are retrieved.
% checkout(ReferenceId) –> ok.
