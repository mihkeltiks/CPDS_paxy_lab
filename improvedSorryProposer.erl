-module(improvedSorryProposer).
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
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal, length(Acceptors), 0) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, length(Acceptors), 0) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(N, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount) ->
    if N =< 0 ->
        {accepted, Proposal};
       SorryCount > (TotalAcceptors - N) ->
        abort;
       true ->
        receive 
            {promise, Round, _, na} ->
                collect(N - 1, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount);
            {promise, Round, Voted, Value} ->
                case order:gr(Voted, MaxVoted) of
                    true ->
                        collect(N - 1, Round, Voted, Value, TotalAcceptors, SorryCount);
                    false ->
                        collect(N - 1, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount)
                end;
            {promise, _, _, _} ->
                collect(N, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount);
            {sorry, {prepare, _}} ->
                collect(N, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount + 1);
            {sorry, _} ->
                collect(N, Round, MaxVoted, Proposal, TotalAcceptors, SorryCount)
        after ?timeout ->
            abort
        end
    end.

vote(N, Round, TotalAcceptors, SorryCount) ->
    if N =< 0 ->
        ok;
       SorryCount > (TotalAcceptors - N) ->
        abort;
       true ->
        receive
            {vote, Round} ->
                vote(N - 1, Round, TotalAcceptors, SorryCount);
            {vote, _} ->
                vote(N, Round, TotalAcceptors, SorryCount);
            {sorry, {accept, _}} ->
                vote(N, Round, TotalAcceptors, SorryCount + 1);
            {sorry, _} ->
                vote(N, Round, TotalAcceptors, SorryCount)
        after ?timeout ->
            abort
        end
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
    if is_tuple(Name) -> 
        Name ! Message;
    true -> 
        case whereis(Name) of
            undefined ->
                down;
            Pid ->
                Pid ! Message
        end
    end.
