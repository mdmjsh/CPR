-module(db).
-export([new/0, destroy/1, match/2, write/3, new/0]).

% Key value store -
% Create a DB which can store, retrieve and delete elemenets.
% Don't use the lists library module

new() -> [].
destroy(DbRef) -> ok.
write(Key, Element, DbRef) ->
    [{Key, Element} | DbRef].
% read(Key, DbRef) →{ok, Element} | {error, instance}.


match(Element, Db) -> match(Element, Db, []).
match(Element, [{Key, Element}| Tail], Acc) ->
  match(Element, Tail, [Key | Acc]);
match(Element, [_|Tail], Acc) ->
  match(Element, Tail, Acc).