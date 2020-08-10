-module(cart).
-behaviour(gen_statem).
-export([start_link/1, shopping/3, payment/3, delivery/3]).
-export([add/2, remove/2, checkout/1, address/2, credit_card/3, delivered/1]).
% -export([checkout/2]).
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

checkout(Ref) when is_integer(Ref)->
    % Used to transition from payment -> delivery states
    [{CCNumber, R, {ExpMo, ExpYr}}] = storage:get_cc(Ref),
    Total = storage:get_total(R),
    [Address] = storage:get_address(R),
    Response = case cc:transaction(Address, CCNumber,
        {ExpMo, ExpYr}, Total) of
        {ok, TrxId} ->
            gen_statem:cast(?NAME, {checkout, TrxId, R}),
            ok;
        {error, funds} -> {error, credit_info};
        {error, invalid_card} -> {error, billing_info}
    end,
    Response;

checkout(ReferenceId) ->
    % Used to transition from shopping -> payment states
    gen_statem:call(?NAME, {checkout, ReferenceId}),
    ok .

address(ReferenceId, Address) ->
    gen_statem:cast(?NAME, {address, [ReferenceId, Address]}),
    ok .


credit_card(ReferenceId, CCNumber, {ExpMo,ExpYr}) ->
    E = {error, card_invalid},
    [Address] = storage:get_address(ReferenceId),
    Response = case cc:is_valid(Address, CCNumber, {ExpMo, ExpYr}) of
        true ->
            gen_statem:call(?NAME, {ReferenceId, CCNumber, {ExpMo,ExpYr}}),
            ok;
        false ->
            gen_statem:cast(?NAME, E),
            E
        end,
    Response .

delivered(ReferenceId) ->
    gen_statem:cast(?NAME, {ReferenceId}),
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

get_state() ->
    sys:get_state(self()).

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

payment({call, From}, {ReferenceId, CCNumber, {ExpMo,ExpYr}}, _) ->
    io:format("Card details valid... ~n"),
    storage:add_card_details(ReferenceId, CCNumber, {ExpMo,ExpYr}),
    gen_statem:reply(From, ok),
    {keep_state, {}} ;

payment(cast, {checkout, TrxId, ReferenceId}, {}) ->
    {next_state, delivery, []}.

delivery(enter, TrxId, _) ->
    io:format("In state: delivery  ~n"),
    {keep_state,  TrxId, {timeout, 1500, refund}} ;

delivery(timeout, TrxId,  _) ->
    io:format("Timeout elasped, issueing refund... ~n"),
    Response = cc:cancel(TrxId),
    {keep_state, refunded, Response};

delivery(cast, {delivered, {ReferenceId}}, {}) ->
    io:format("Order ~p delivered - enjoy! ~n", [ReferenceId]),
    stop() .