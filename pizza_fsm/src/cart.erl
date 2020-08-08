-module(cart).
-behaviour(gen_statem).
-export([start_link/1, shopping/3, payment/3, delivery/3]).
-export([add/2, remove/2, checkout/1, address/2, credit_card/3, delivered/1]).
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

checkout(Ref) when is_integer(Ref) ->
    % Used to transition from payment -> delivery states
    gen_statem:call(?NAME, {checkout, check_payment_details, Ref}),
    ok ;

checkout(ReferenceId) ->
    % Used to transition from shopping -> payment states
    gen_statem:call(?NAME, {checkout, ReferenceId}),
    ok .


address(ReferenceId, Address) ->
    % Example Address format:
    % [{address, {number, "StreetName"}
    % {name, "Name of client"}
    % {city, "City Name"}
    % {country, "Country"}}]
    % [{_, {Number, Street}, {_, Name}}]
    gen_statem:cast(?NAME, {address, [ReferenceId, Address]}),
    ok .

credit_card(ReferenceId, CCNumber, {ExpMo,ExpY}) ->
    gen_statem:call(?NAME, {credit_card, ReferenceId, CCNumber, {ExpMo,ExpY}}),
    checkout(CCNumber),
    ok .

delivered(ReferenceId) ->
    gen_statem:call(?NAME, {ReferenceId}),
    ok .


%% Callback implementations
%% ------------------------
callback_mode() ->
    % http://erlang.org/documentation/doc-10.3/doc/design_principles/statem.html#State%20Enter%20Calls
    [state_functions, state_enter].

init(ReferenceId) ->
    process_flag(trap_exit, true),
    io:format("starting cart... ~n"),
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
    {keep_state, {}} ;

shopping({call, From}, {checkout, ReferenceId}, _) ->
    io:format("Going to checkout  ~p~n",[ReferenceId]),
    gen_statem:reply(From, ok),
    {next_state, payment, []}.

payment(enter, _, _) ->
    io:format("In state: payment  ~n"),
    {keep_state, {}} ;

payment(cast, {address, [ReferenceId, [{_, {Number, Street},
    {_, Name}, {_, City},{_, Country}}]]}, {}) ->
    storage:add_address(ReferenceId, Country, City, Street, Number, Name),
    {keep_state, {}} ;

payment({call, From}, {credit_card, ReferenceId, CCNumber, {ExpMo,ExpY}}, _) ->
    storage:add_card_details(ReferenceId, ReferenceId, CCNumber, {ExpMo,ExpY}),
    gen_statem:reply(From, ok),
    {keep_state, {}} ;
    % {next_state, delivery, []}.

payment({call, From}, {checkout, check_payment_details, ReferenceId}, _) ->
    io:format("Checking card details... ~n"),
    gen_statem:reply(From, ok),
    {next_state, delivery, []}.

delivery(enter, _, _) ->
    io:format("In state: delivery  ~n"),
    {keep_state, {}} .

%  delivery({timeout, Time, ReferenceId} ) ->
%      io:format("Enjoy!  ~n"),
%      {keep_state, {}} .