-module(maps_).
-export([make_pizza/2]).


assemble_pizza(Rest) ->
    [cheese, tomato | Rest].

pizza_toppings(margarita) ->
    assemble_pizza([]);

pizza_toppings(funghi) ->
    assemble_pizza([mushroom]);

pizza_toppings(american_hot) ->
    assemble_pizza([pepperoni, jalepenos]).

fill_quanties(Map, [H | T], [H_ | T_]) ->
    Map1 = Map#{H => H_},
    print(H, H_),
    fill_quanties(Map1, T, T_);

fill_quanties(Map, [], []) ->
    Map.


make_pizza(PizzaName, Quantities) ->
    fill_quanties(#{ }, pizza_toppings(PizzaName), Quantities).

print(Pid, Verb) ->  io:format("~w: ~w ~n", [Pid, Verb]), ok.