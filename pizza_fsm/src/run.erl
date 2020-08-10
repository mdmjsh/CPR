-module(run).
-export([setup/0, full_run/0, bad_card/0, delivery_timeout/0]).
-export([shop/0, add_address/1, add_payment/1, deliver/1, add_payment/2, deliver/2]).


%% Helpers
%% ------------------------

trap_retry(Func) ->
    process_flag(trap_exit, true),
    R = try Func()
        catch exit: _ -> Func()
    end,
    R .

trap_retry(Func, A) ->
    process_flag(trap_exit, true),
    R = try Func(A)
        catch exit: _ -> Func(A)
    end,
    R .

trap_retry(Func, A, B) ->
    process_flag(trap_exit, true),
    R = try Func(A, B)
        catch exit: _ -> Func(A, B)
    end,
    R .

trap_retry(Func, A, B, C) ->
    process_flag(trap_exit, true),
    R = try Func(A, B, C)
        catch exit: _ -> Func(A, B, C)
    end,
    R .

setup() ->
    R =  trap_retry(fun shop/0),
    trap_retry(fun add_address/1, R),
    R .



%% Full scenarios
%% ------------------------

full_run() ->
    process_flag(trap_exit, true),
    R = setup(),
    trap_retry(fun add_payment/1, R),
    trap_retry(fun deliver/1, R),
    R.

bad_card() ->
    process_flag(trap_exit, true),
    R = setup(),
    add_payment(R, bad),
    R.

delivery_timeout() ->
    process_flag(trap_exit, true),
    R = setup(),
    add_payment(R),
    deliver(R, bad),
    R.



%% State operations
%% ------------------------

shop() ->
    process_flag(trap_exit, true),
    storage:initDB([node()]),
    {_, R} = cart:start_link(a),
    cart:add(R, {pizza, cheese}),
    cart:add(R, {pizza, mushroom}),
    cart:add(R, {pizza, mushroom}),
    cart:remove(R, {pizza, mushroom}),
    cart:checkout(R),
    R .


add_address(R) ->
    F = fun cart:address/2,
    trap_retry(F, R, [{address, {54, "Some road"},
        {name, "Bob Jones"},
        {city, "London"},
        {country, "England"}}]
        ),
    % cart:address(
    %     R,
        % [{address, {54, "Some road"},
        % {name, "Bob Jones"},
        % {city, "London"},
        % {country, "England"}}]
        % ) .
        R .

add_payment(R) -> add_payment(R, good) .

add_payment(R, good) ->
    C = 5500005555555559,
    % F1 = fun cart:credit_card/3,
    % F2 = fun cart:checkout/1,
    % F3 = fun cart:delivered/1,

    % trap_retry(F1, R, C, {08, 21}),
    % trap_retry(F2,C),
    % trap_retry(F3,R),

    cart:credit_card(R, C, {08, 21}),
    cart:checkout(C),
    cart:delivered(R),
    R ;

add_payment(R, bad) ->
    % F = fun cart:credit_card/3,
    cart:credit_card(
        R,
        1234465789102383362783, {08, 21}
        ),
    R .
    % trap_retry(F, [R, 1234465789102383362783, {08, 21}]),
    % R .



deliver(R) -> deliver(R, good).

deliver(R, good) ->
    cart:delivered(R);

deliver(R, bad) ->
    timer:sleep(2000),
    F = fun cart:delivered/1,
    trap_retry(F, R).
    % cart:delivered(R) .
