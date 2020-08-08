-module(run).
-export([rabbit/0]).

rabbit() ->
    storage:initDB([node()]),
    {_, Reference} = cart:start_link(a),
    cart:add(Reference, {pizza, cheese}),
    cart:add(Reference, {pizza, mushroom}),
    cart:add(Reference, {pizza, mushroom}),
    cart:remove(Reference, {pizza, mushroom}),
    cart:checkout(Reference),

    cart:address(
        Reference,
        [{address, {54, "Some road"},
        {name, "Bob Jones"},
        {city, "London"},
        {country, "England"}}]
        ) ,

    cart:credit_card(
        Reference,
        12344657891023833, {08, 21}
        ) .
    % cart:delivered(Reference).