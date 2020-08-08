-module(storage).

-export([initDB/1, add_item/2, remove_item/2, get_cart/1, lookup/1, load_menu/0]).

-record(cartDB, {referenceID, items=[], total=0}).
-record(menu, {itemId, type, item, string, price}).

% pass in list of nodes, e.g. [node()] to run DB on
initDB(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    % try to call the table info, and if fails
    % (i.e table doesn't exist, create table)
    try
        mnesia:table_info(type, cartDB),
        mnesia:table_info(type, menu)
    catch
        exit: _ ->
            io:format("initialising Mnesia.... ~n"),
            mnesia:create_table(cartDB, [{attributes, record_info(fields, cartDB)},
                {type, ordered_set},
                {disc_copies, Nodes}]),
            mnesia:create_table(menu, [{attributes, record_info(fields, menu)},
                {type, ordered_set},
                {ram_copies, Nodes}]),
            load_menu()
    end.

load_menu() ->
    io:format("Loading Menu.... ~n"),
    {_, Dir} = file:get_cwd(),
    {_, [Menu]} = file:consult(Dir ++ "/src" ++ "/menu.txt"),
    load_menu(Menu) .

load_menu([H | T]) ->
    {Type, Item, String, Price} = H ,
    Id = atom_to_list(Type) ++ atom_to_list(Item),
    F = fun() ->
        mnesia:write(#menu{itemId=Id, type=Type, item=Item,
            string=String, price=Price})
    end,
    io:format("Saving {~p,~p} to Mnesia... ~n", [Type, Item]),
    mnesia:transaction(F),
    load_menu(T) ;

load_menu([]) ->
    io:format("Menu loaded. ~n"),
    {ok} .


add_item(ReferenceId, init) ->
    io:format("Creating empty cart ~p... ~n", [ReferenceId]),
    F = fun() ->
        mnesia:write(#cartDB{referenceID=ReferenceId, items=[], total=0})
    end,
    mnesia:transaction(F);

add_item(ReferenceId, Item)  ->
    {Item_, ItemPrice} = lookup(Item),
    [Cart] = get_cart(ReferenceId),
    io:format("Found cart ~p ~n", [Cart]),
    Items = lists:append(Cart#cartDB.items,  [Item_]),
    Total = Cart#cartDB.total + ItemPrice,

    F = fun() ->
        mnesia:write(#cartDB{referenceID=ReferenceId, items=Items, total=Total})
    end,
    io:format("Cart now includes ~p... ~n", [Items]),
    io:format("Cart total is now ~p... ~n", [Total]),
    mnesia:transaction(F).


remove_item(ReferenceId, Item)  ->
    {Item_, ItemPrice} = lookup(Item),
    [Cart] = get_cart(ReferenceId),
    io:format("Found cart ~p ~n", [Cart]),
    Items = lists:delete(Item_, Cart#cartDB.items),
    Total = Cart#cartDB.total + ItemPrice,

    F = fun() ->
        mnesia:write(#cartDB{referenceID=ReferenceId, items=Items, total=Total})
    end,
    io:format("Cart now includes ~p... ~n", [Items]),
    io:format("Cart total is now ~p... ~n", [Total]),
    mnesia:transaction(F).

lookup(Item) ->
    io:format("Retrieving ~p from menu... ~n", [Item]),
    F = fun() ->
        mnesia:read(menu, Item)
    end,
    {_, [Results]} = mnesia:transaction(F),
    io:format("Found record ~p ~n", [Results]),
    {Results#menu.itemId, Results#menu.price} .


get_cart(ReferenceId) ->
    F = fun() ->
        mnesia:read({cartDB, ReferenceId})
    end,
    {_, Results} = mnesia:transaction(F),
    Results .