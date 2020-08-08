-module(cc).
-export([is_valid/3]).

is_valid(Address, CCNumber, {ExpMo,ExpYr}) ->
    ok.

transaction(Address, CCNumber, {ExpMo,ExpYr}, Price) ->
            %    {ok, TrxId} . {error, invalid_card} | {error, funds}
            ok.

cancel(TrxId) ->
    %  ok | {error, unknown}
    ok.