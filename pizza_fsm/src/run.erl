-module(run).
-export([rabbit/0]).

rabbit() ->
    % Menu = load_menu(),
    storage:initDB([node()]),
    {_, Reference} = cart:start_link(a),
    cart:add(Reference, {pizza, cheese}),
    cart:add(Reference, {pizza, mushroom}),
    cart:add(Reference, {pizza, mushroom}),
    cart:remove(Reference, {pizza, mushroom}).