-module(run).
-export([run/0]).
% -export([shopping/0, payment/1]).

run() ->
    % shopping
    storage:initDB([node()]),
    {_, R} = cart:start_link(a),
    cart:add(R, {pizza, cheese}),
    cart:add(R, {pizza, mushroom}),
    cart:add(R, {pizza, mushroom}),
    cart:remove(R, {pizza, mushroom}),
    cart:checkout(R),
    R .

shopping(R) ->
    % payment
    cart:address(
        R,
        [{address, {54, "Some road"},
        {name, "Bob Jones"},
        {city, "London"},
        {country, "England"}}]
        ) ,

    % % invalid card
    % cart:credit_card(
    %     R,
    %     1234465789102383362783, {08, 21}
    %     ) ,
    % valid card
    C = 5500005555555559,
    cart:credit_card(R, C, {08, 21}),
    cart:checkout(C),
    R .
