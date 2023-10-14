-module(acceptor).
-export([start/2]).
-define(delay, 200).
-define(drop, 1).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  Promised = order:null(),
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  T = rand:uniform(?delay), % defining a time T
  P = rand:uniform(10),
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          if P =< ?drop -> % if the drop P is 1 or less, then send the message
               io:format("message dropped~n");
             true ->
               timer:send_after(T, Proposer, {promise, Round, Voted, Value})
          end,
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
            [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Promised}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of %
        true ->
          %timer:send_after(T, Proposer, {vote, Round}), %
          if P =< ?drop -> % if the drop P is 1 or less, then send the message
               io:format("message dropped~n");
             true ->
               timer:send_after(T, Proposer, {vote, Round})
          end,
          case order:goe(Round, Voted) of % slide 9: because I had voted for something before but the new value is higher
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]), %
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), %
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal}, % proposal is the new colour
              acceptor(Name, Promised, Round, Proposal, PanelId); %
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId) %
          end;
        false ->
          %Proposer ! {sorry, {accept, Voted}}, %
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
