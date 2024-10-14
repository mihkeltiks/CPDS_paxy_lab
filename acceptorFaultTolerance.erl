-module(acceptorFaultTolerance).
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
    pers:open(Name),
    PrNull = order:null(),
    case pers:read(Name) of
        {Pr, Vt, Ac, Pn} when Pr =/= PrNull ->
            %% Recovered state
            io:format("[Acceptor ~w] Recovered state~n", [Name]),
            %% Use recovered PanelId if PanelId is 'na'
            PanelId1 = case PanelId of
                           na -> Pn;
                           _ -> PanelId
                       end,
            %% Update GUI with recovered state
            Colour = case Ac of
                         na -> {0,0,0};
                         _ -> Ac
                     end,
            PanelId1 ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Vt]),
                        "Promised: " ++ io_lib:format("~p", [Pr]), Colour},
            acceptor(Name, Pr, Vt, Ac, PanelId1);
        _ ->
            %% Initialize state
            Promised = order:null(),
            Voted = order:null(),
            Value = na,
            acceptor(Name, Promised, Voted, Value, PanelId)
    end.



acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive 
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    % Update state
                    Promised1 = Round,
                    % Save state
                    pers:store(Name, Promised1, Voted, Value, PanelId),
                    % Send promise
                    send_maybe_drop(Proposer, {promise, Round, Voted, Value}),
                    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                              [Name, Round, Voted, Value]),
                    % Update GUI
                    Colour = case Value of na -> {0,0,0}; _ -> Value end,
                    PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                               "Promised: " ++ io_lib:format("~p", [Round]), Colour},
                    acceptor(Name, Promised1, Voted, Value, PanelId);
                false ->
                    send_maybe_drop(Proposer, {sorry, {prepare, Round}}),
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promised) of
                true ->
                    % Update state
                    Voted1 = Round,
                    Value1 = Proposal,
                    % Save state
                    pers:store(Name, Promised, Voted1, Value1, PanelId),
                    % Send vote
                    send_maybe_drop(Proposer, {vote, Round}),
                    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                              [Name, Promised, Round, Proposal]),
                    % Update GUI
                    PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                               "Promised: " ++ io_lib:format("~p", [Promised]), Value1},
                    acceptor(Name, Promised, Voted1, Value1, PanelId);
                false ->
                    send_maybe_drop(Proposer, {sorry, {accept, Round}}),
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        stop ->
            % Remove persisted state
            pers:delete(Name),
            pers:close(Name),
            PanelId ! stop,
            ok
    end.
