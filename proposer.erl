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
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of % due to the ballot function bellow
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
  Sorry_Count = Quorum,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal, Sorry_Count) of % collect N promises (and control the amount of sorry messages)
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors), %
      case vote(Quorum, Round, Sorry_Count) of % sends the quorum to control the amount of sorry messages
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
collect(N, Round, _, _, 0) -> % the majority of the Acceptors answered sorry, so abort the current Round
  io:format("[Sorry message for Prepare:] Round ~w*********~n", [Round]),
  abort;
collect(N, Round, MaxVoted, Proposal, Sorry_Count) ->
  receive
    {promise, Round, _, na} -> % is the first answer, at the beginning of the execution, so we already start decrementing
      collect(N-1, Round, MaxVoted, Proposal, Sorry_Count); % There wasn't any previous promise, so the acceptor promised on Round
    {promise, Round, Voted, Value} ->
      case order:gr(Voted, MaxVoted) of % There was a previous MaxVoted, that could be bigger or not than Voted
        true ->
          collect(N-1, Round, Voted, Value, Sorry_Count);
        false ->
          collect(N-1, Round, MaxVoted, Proposal, Sorry_Count)
      end;
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal, Sorry_Count);
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal, Sorry_Count); % Does not decrease N cause one of the acceptors didn't promise on it
                                                      % Decreases the Sorry_Count quorum to verify when the majority could not promise on a value
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal, Sorry_Count)
  after ?timeout ->
    abort
  end.

vote(0, _, _) ->
  ok;
vote(N, Round, 0) -> % the majority of the Acceptors answered sorry, so abort the current Round
  io:format("[Sorry message for Accept:} Round ~w*********~n", [Round]),
  abort;
vote(N, Round, Sorry_Count) ->
  receive
    {vote, Round} ->
      vote(N-1, Round, Sorry_Count); % I decrement because they voted for Round
    {vote, _} ->
      vote(N, Round, Sorry_Count);
    {sorry, {accept, Round}} ->
      vote(N, Round, Sorry_Count); % I don't decrement N because they didn't vote for Round
                               % Decreases the Sorry_Count quorum to verify when the majority could not promise on a value
    {sorry, _} ->
      vote(N, Round, Sorry_Count)
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
