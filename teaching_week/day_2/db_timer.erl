% PATTERN MATCHING
match(Element, Db) -> match_helper(Element, Db, []).
match_helper(_, [], Acc) -> Acc;
match_helper(Element, [{Key, Element}|Tail], Acc) ->
  match_helper(Element, Tail, [Key | Acc]);
match_helper(Element, [_|Tail], Acc) ->
  match_helper(Element, Tail, Acc).

% IF STATEMENT
match_2(Element, Db) -> match_helper_2(Element, Db, []).
match_helper_2(_, [], Acc) -> Acc;
match_helper_2(Element, [Head|Tail], Acc) ->
  if
    Element == element(2, Head) ->
      match_helper_2(Element, Tail, [element(1, Head) | Acc]);
    _ -> match_helper_2(Element, Tail, Acc)
  end.

% CASE CLAUSE
match_3(Element, Db) -> match_helper_3(Element, Db, []).
match_helper_3(_, [], Acc) -> Acc;
match_helper_3(Element, [Head|Tail], Acc) ->
  case element(2, Head) of
    Element -> match_helper_3(Element, Tail, [element(1, Head) | Acc]);
    _       -> match_helper_3(Element, Tail, Acc)
  end.

T = db:create(100000), D = db:new(), D2 = db:extend(T, D), {Q,W,E} = erlang:timestamp(), db:match(2, D2), {Q2,W2,E2} = erlang:timestamp(), timer:now_diff({Q2,W2,E2}, {Q,W,E}).

T = db:create(100000), D = db:new(), D2 = db:extend(T, D), {Q,W,E} = erlang:timestamp(), db:match_2(2, D2), {Q2,W2,E2} = erlang:timestamp(), timer:now_diff({Q2,W2,E2}, {Q,W,E}).

T = db:create(100000), D = db:new(), D2 = db:extend(T, D), {Q,W,E} = erlang:timestamp(), db:match_3(2, D2), {Q2,W2,E2} = erlang:timestamp(), timer:now_diff({Q2,W2,E2}, {Q,W,E}).

