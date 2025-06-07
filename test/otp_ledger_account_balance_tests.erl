-module(otp_ledger_account_balance_tests).

-include_lib("eunit/include/eunit.hrl").

%% Setup and teardown helpers
setup() ->
  {ok, Pid} = otp_ledger_sup:start_link(),
  Pid.

cleanup(Pid) ->
  unlink(Pid),
  exit(Pid, shutdown).

%% Test generator with setup and teardown
account_balance_test_() ->
  {setup,
   fun setup/0,
   fun cleanup/1,
   fun(_) ->
      [test_start_and_get_pid(),
       test_get_balance(),
       test_credit_debit(),
       test_insufficient_funds(),
       test_apply_events()]
   end}.

%% Individual test cases
test_start_and_get_pid() ->
  {"Account projectors can be started and retrieved",
   fun() ->
      {ok, Pid1} = otp_ledger_account_balance:start_link("test1"),
      ?assert(is_pid(Pid1)),
      {ok, Pid2} = otp_ledger_account_balance:get_pid("test1"),
      ?assertEqual(Pid1, Pid2)
   end}.

test_get_balance() ->
  {"Account balance starts at zero",
   fun() ->
      {ok, _} = otp_ledger_account_balance:start_link("test2"),
      ?assertEqual(0, otp_ledger_account_balance:get_balance("test2"))
   end}.

test_credit_debit() ->
  {"Account can be credited and debited",
   fun() ->
      {ok, _} = otp_ledger_account_balance:start_link("test3"),
      ?assertEqual(0, otp_ledger_account_balance:get_balance("test3")),

      % Credit 100
      ?assertEqual(100, otp_ledger_account_balance:credit("test3", 100)),
      ?assertEqual(100, otp_ledger_account_balance:get_balance("test3")),

      % Debit 30
      ?assertEqual(70, otp_ledger_account_balance:debit("test3", 30)),
      ?assertEqual(70, otp_ledger_account_balance:get_balance("test3"))
   end}.

test_insufficient_funds() ->
  {"Debit fails when insufficient funds",
   fun() ->
      {ok, _} = otp_ledger_account_balance:start_link("test4"),
      ?assertEqual(50, otp_ledger_account_balance:credit("test4", 50)),
      ?assertEqual({error, insufficient_funds}, otp_ledger_account_balance:debit("test4", 100)),
      ?assertEqual(50, otp_ledger_account_balance:get_balance("test4"))
   end}.

test_apply_events() ->
  {"Events can be applied to update balance",
   fun() ->
      % Create an event to deposit 100
      DepositEvent =
        #{account_number => "test5",
          event_type => amount_deposited,
          value => 100},
      otp_ledger_account_balance:apply_event(DepositEvent),
      ?assertEqual(100, otp_ledger_account_balance:get_balance("test5")),

      % Create an event to withdraw 30
      WithdrawEvent =
        #{account_number => "test5",
          event_type => amount_withdrawn,
          value => 30},
      otp_ledger_account_balance:apply_event(WithdrawEvent),
      ?assertEqual(70, otp_ledger_account_balance:get_balance("test5")),

      % Create an event to apply a fee of 5
      FeeEvent =
        #{account_number => "test5",
          event_type => fee_applied,
          value => 5},
      otp_ledger_account_balance:apply_event(FeeEvent),
      ?assertEqual(65, otp_ledger_account_balance:get_balance("test5")),

      % Create an unknown event type that should be ignored
      UnknownEvent =
        #{account_number => "test5",
          event_type => unknown_event,
          value => 1000},
      otp_ledger_account_balance:apply_event(UnknownEvent),
      ?assertEqual(65, otp_ledger_account_balance:get_balance("test5"))
   end}.
