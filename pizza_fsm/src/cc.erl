-module(cc).
-export([is_valid/3]).
-export([check_expiry/3, check_address/2, check_funds/2]).
-export([transaction/4, cancel/1]).

check_expiry(_, ExpMo, ExpYr) ->
    Valid = ExpMo + ExpYr < 44,
    Valid .


check_address(_, [{_, {Number, _}, {_, _},{_, _},{_, _}}]) ->
    Valid = Number >= 10,
    Valid .

check_funds(_, Amount) ->
    Valid = Amount < 200,
    Valid .

is_valid(Address, CCNumber, {ExpMo,ExpYr}) ->
    Valid = case erlcard:valid_credit_card(CCNumber) of
        {ok, _, _} ->
            ValidExpiry = check_expiry(CCNumber, {ExpMo, ExpYr}),
            ValidAddress = check_address(CCNumber, Address),
            ValidExpiry andalso ValidAddress;
        false ->
            false
        end,
        Valid .

transaction(Address, CCNumber, {ExpMo,ExpYr}, Price) ->
    case is_valid(Address, CCNumber, {ExpMo,ExpYr})
        of true ->
            case check_funds(CCNumber, Price) of
                true ->
                    Response = {ok, uuid:to_string(uuid:uuid4())};
                false ->
                    Response = {error, funds}
                end;
        false ->
            Response = {error, invalid_card}
        end,
    Response .

% https://stackoverflow.com/a/31599075/4028718 - setting Response around a try
cancel(TrxId) ->
    Response = try
        case uuid:is_valid(TrxId)
            of true ->
                {ok};
            false ->
                {error, unknown}
            end
    catch
    % error:{badarg, _}
       error: _ ->
            {error, unknown}
    end ,
    Response .