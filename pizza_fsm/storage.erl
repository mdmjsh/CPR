-module(storage).

-export([initDB/1, add_item/2, get_cart/1, lookup/1, load_menu/0]).

-record(cartDB, {referenceID, items=[], total=0}).
-record(menu, {itemId, item, type, string, price}).

% pass in list of nodes, e.g. [node()] to run DB on
initDB(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    % try to call the table info, and if fails
    % (i.e table doesn't exist, create table)
    try
        mnesia:table_info(type, cartDB)
    catch
        exit: _ ->
            mnesia:create_table(cartDB, [{attributes, record_info(fields, cartDB)},
                {type, ordered_set},
                {disc_copies, [node()]}]),
            mnesia:create_table(menu, [{attributes, record_info(fields, menu)},
                {type, ordered_set},
                {ram_copies, [node()]}]),
            load_menu()
    end.

load_menu([{Type, Item, String, Price} | T ]) ->
    Id = atom_to_list(Type) ++ atom_to_list(Item),
    F = fun() ->
        mnesia:write(#menu{itemId=Id, type=Type, string=String, price=Price})
    end,
    mnesia:transaction(F),
    load_menu(T) ;


load_menu([]) ->
    {ok} .

load_menu() ->
    {_, [Menu]} = file:consult("src/menu.txt"),
    load_menu(Menu) .

add_item(ReferenceId, Item) ->
    Item, ItemPrice = lookup(Item),
    [CurrentBasket] = get_cart(ReferenceId),
    Items = CurrentBasket#cartDB.items ++ Item,
    Total = CurrentBasket#cartDB.total + ItemPrice,

    F = fun() ->
        mnesia:write(#cartDB{referenceID=ReferenceId, items=Items, total=Total})
    end,
    mnesia:transaction(F).


lookup(Item) ->
    F = fun() ->
        mnesia:read(menu, Item)
    end,
    [Results] = mnesia:transaction(F),
    {Results#menu.item, Results#menu.price} .


get_cart(ReferenceId) ->
    F = fun() ->
        mnesia:read({cartDB, ReferenceId})
    end,
    [Results] = mnesia:transaction(F),
    Results .