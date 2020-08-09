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

    % invalid card
    cart:credit_card(
        Reference,
        "12344657891023833", {08, 21}
        ) ,

    cart:credit_card(Reference, "5500005555555559", {08, 21}).
    % cart:delivered(Reference).