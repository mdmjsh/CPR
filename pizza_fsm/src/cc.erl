-module(cc).
-export([is_valid/3]).
-export([transaction/4, cancel/1]).

check_expiry(_, {ExpMo, ExpYr}) ->
    Valid = ExpMo + ExpYr < 44,
    Valid .

check_address(_, {_, _, _, Number, _}) ->
    Valid = Number >= 10,
    Valid .

check_funds(_, Amount) ->
    Valid = Amount < 200,
    Valid .

check_credit_card(CCNumber) ->
    Valid = length(integer_to_list(CCNumber)) < 17,
    Valid.

is_valid(Address, CCNumber, {ExpMo,ExpYr}) ->
    Valid = case check_credit_card(CCNumber) of
        true ->
            io:format("Validating: ~p, {~p, ~p} ~n", [CCNumber, ExpMo, ExpYr]),
            ValidExpiry = check_expiry(CCNumber, {ExpMo, ExpYr}),
            ValidAddress = check_address(CCNumber, Address),
            io:format("Valid Expiry: ~p, Valid Address: ~p ~n", [ValidExpiry, ValidAddress]),
            ValidExpiry andalso ValidAddress;
        false ->
            false
        end,
        Valid .

transaction(Address, CCNumber, {ExpMo,ExpYr}, Price) ->
    Response = case is_valid(Address, CCNumber, {ExpMo,ExpYr})
        of true ->
            case check_funds(CCNumber, Price) of
                true -> {ok, uuid:to_string(uuid:uuid4())};
                false -> {error, funds}
                end;
        false -> {error, invalid_card}
        end,
    Response .

% https://stackoverflow.com/a/31599075/4028718 - setting Response around a try
cancel(TrxId) ->
    io:format("cancalling transaction: ~p~n", [TrxId]),
    Response = try
        case uuid:is_valid(TrxId)
            of true -> {ok};
            false -> {error, unknown}
            end
    catch
       error: _ -> {error, unknown}
    end ,
    Response .