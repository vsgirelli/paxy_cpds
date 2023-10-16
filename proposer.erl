-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of % due to the ballot function below
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round), %
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId) %
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors), %
  Quorum = (length(Acceptors) div 2) + 1, %
  MaxVoted = order:null(),
  SorryCounter = length(Acceptors) - Quorum, %Initialized sorry counter
  case collect(Quorum, Round, MaxVoted, Proposal, SorryCounter) of % collect N promises
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors), %
      case vote(Quorum, Round, SorryCounter) of %
        ok ->
          {ok, Value}; %
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0, _, _, Proposal, _) -> % reached the Quorum
  {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal, SorryCounter) ->
  receive
    {promise, Round, _, na} -> % is the first answer, at the beginning of the execution, so we already start decrementing
      collect(N-1, Round, MaxVoted, Proposal, SorryCounter); % There wasn't any previous promise, so the acceptor promised on Round
    {promise, Round, Voted, Value} ->
      case order:gr(MaxVoted, Voted) of % There was a previous MaxVoted, that could be bigger or not than Voted
        true ->
          collect(N-1, Round, MaxVoted, Proposal, SorryCounter);
        false ->
          collect(N-1, Round, Voted, Value, SorryCounter)
      end;
    {promise, _, _, _} ->
      collect(N, Round, MaxVoted, Proposal, SorryCounter-1);
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal, SorryCounter-1); % Decrease SorryCounter in the sorry message during the promise phase
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal, SorryCounter-1)
  after ?timeout ->
    abort
  end.

vote(0, _, _) ->
  ok;
vote(N, Round, SorryCounter) ->
  receive
    {vote, Round} ->
      vote(N-1, Round, SorryCounter); % I decrement because they voted for Round
    {vote, _} ->
      vote(N, Round, SorryCounter);
    {sorry, {accept, Round}} ->
      vote(N, Round, SorryCounter-1); % I don't decrement because they didn't vote for Round
    {sorry, _} ->
      vote(N, Round, SorryCounter-1)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
