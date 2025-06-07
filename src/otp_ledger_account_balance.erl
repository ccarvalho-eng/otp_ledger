%%%-------------------------------------------------------------------
%% @doc Account balance projector
%% @end
%%%-------------------------------------------------------------------

-module(otp_ledger_account_balance).

-behaviour(gen_server).

%% API
-export([start_link/1, get_pid/1, get_balance/1, credit/2, debit/2, apply_event/1,
         apply_event/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SCOPE, account_projectors).

%%====================================================================
%% API
%%====================================================================
%% Start a new account balance projector for the given account number
start_link(AccountNumber) ->
  % Use unique name based on account number
  Name = list_to_atom("account_balance_" ++ AccountNumber),
  % Start the gen_server with a local name
  {ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, AccountNumber, []),
  % Register the process with pg
  ok = pg:join(?SCOPE, AccountNumber, Pid),
  {ok, Pid}.

%% Get the PID for an account's projector
get_pid(AccountNumber) ->
  case pg:get_members(?SCOPE, AccountNumber) of
    [Pid | _] ->
      {ok, Pid};
    [] ->
      {error, not_found}
  end.

%% Apply event to an account
apply_event(#{account_number := AccountNumber} = Event) when is_list(AccountNumber) ->
  case get_pid(AccountNumber) of
    {ok, Pid} ->
      apply_event(Pid, Event);
    {error, not_found} ->
      logger:debug("Attempt to apply event to non-existent account, starting projector"),
      {ok, Pid} = start_link(AccountNumber),
      apply_event(Pid, Event)
  end.

%% Apply event directly to a projector process
apply_event(Pid, Event) when is_pid(Pid) ->
  gen_server:cast(Pid, {handle_event, Event}).

%% Get the current balance for an account
get_balance(AccountNumber) ->
  case get_pid(AccountNumber) of
    {ok, Pid} ->
      gen_server:call(Pid, {get_balance});
    Error ->
      Error
  end.

%% Credit an account (increase balance)
credit(AccountNumber, Amount) when is_number(Amount), Amount > 0 ->
  case get_pid(AccountNumber) of
    {ok, Pid} ->
      gen_server:call(Pid, {credit, Amount});
    Error ->
      Error
  end.

%% Debit an account (decrease balance)
debit(AccountNumber, Amount) when is_number(Amount), Amount > 0 ->
  case get_pid(AccountNumber) of
    {ok, Pid} ->
      gen_server:call(Pid, {debit, Amount});
    Error ->
      Error
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init(AccountNumber) ->
  {ok, #{balance => 0, account_number => AccountNumber}}.

handle_call({get_balance}, _From, State = #{balance := Balance}) ->
  {reply, Balance, State};
handle_call({credit, Amount}, _From, State = #{balance := Balance}) ->
  NewBalance = Balance + Amount,
  {reply, NewBalance, State#{balance := NewBalance}};
handle_call({debit, Amount}, _From, State = #{balance := Balance})
  when Balance >= Amount ->
  NewBalance = Balance - Amount,
  {reply, NewBalance, State#{balance := NewBalance}};
handle_call({debit, _Amount}, _From, State = #{balance := _Balance}) ->
  {reply, {error, insufficient_funds}, State};
handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast({handle_event, Event}, State) ->
  {noreply, handle_event(State, Event)};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Event handling
%%====================================================================

%% Handle amount_withdrawn event
handle_event(#{balance := Balance} = State,
             #{event_type := amount_withdrawn, value := Value}) ->
  State#{balance := Balance - Value};
%% Handle amount_deposited event
handle_event(#{balance := Balance} = State,
             #{event_type := amount_deposited, value := Value}) ->
  State#{balance := Balance + Value};
%% Handle fee_applied event
handle_event(#{balance := Balance} = State,
             #{event_type := fee_applied, value := Value}) ->
  State#{balance := Balance - Value};
%% Default handler for unknown events
handle_event(State, _UnknownEvent) ->
  State.
