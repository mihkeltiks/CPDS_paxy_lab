-module(acceptor).
-export([start/2]).
-define(drop_ratio, 0). % Drops 1 in 10 messages
-define(delay, 1).


send_maybe_drop(Pid, Message) ->
    P = rand:uniform(10),
    if P =< ?drop_ratio ->
        io:format("Message dropped~n"),
        ok;
    true ->
        T = rand:uniform(?delay),
        timer:send_after(T, Pid, Message)
        % Pid ! Message
    end.


start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive 
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          % Proposer  ! {promise, Round, Voted, Value},
          send_maybe_drop(Proposer, {promise, Round, Voted, Value}) ,             
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
         % Proposer  ! {sorry, {prepare, Round}},
         send_maybe_drop(Proposer, {sorry, {prepare, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          % Proposer  ! {vote, Round},
          send_maybe_drop(Proposer, {vote, Round}),
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          % Proposer  ! {sorry, {accept, Round}},
          send_maybe_drop(Proposer, {sorry, {accept, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
