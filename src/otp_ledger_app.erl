%%%-------------------------------------------------------------------
%% @doc otp_ledger public API
%% @end
%%%-------------------------------------------------------------------

-module(otp_ledger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    otp_ledger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
