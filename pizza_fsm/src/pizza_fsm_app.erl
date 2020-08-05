%%%-------------------------------------------------------------------
%% @doc pizza_fsm public API
%% @end
%%%-------------------------------------------------------------------

-module(pizza_fsm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pizza_fsm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
