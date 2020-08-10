-module(run).
-export([run/0]).
-export([shop/0, add_address/1, add_payment/1, deliver/1]).

run() ->
    R = try shop()
    catch exit: _ -> shop()
    end,
    add_address(R),
    add_payment(R),
    deliver(R),
    R.

shop() ->
    % shopping
    storage:initDB([node()]),
    {_, R} = cart:start_link(a),
    cart:add(R, {pizza, cheese}),
    cart:add(R, {pizza, mushroom}),
    cart:add(R, {pizza, mushroom}),
    cart:remove(R, {pizza, mushroom}),
    cart:checkout(R),
    R .


add_address(R) ->
    % payment
    cart:address(
        R,
        [{address, {54, "Some road"},
        {name, "Bob Jones"},
        {city, "London"},
        {country, "England"}}]
        ) .

add_payment(R) ->
    % invalid card
    % cart:credit_card(
    %     R,
    %     1234465789102383362783, {08, 21}
    %     ) ,
    % valid card
    C = 5500005555555559,
    cart:credit_card(R, C, {08, 21}),
    cart:checkout(C),
    R .

deliver(R) ->
    timer:sleep(2000),
    cart:delivered(R).
