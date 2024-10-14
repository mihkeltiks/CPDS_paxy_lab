-module(paxyRemote).
-export([start/1, stop/0, stop/1, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    %% Node names
    AcceptorNode = 'paxy_acc@chomsky',
    ProposerNode = 'paxy_pro@chomsky',

    %% Ensure we can communicate with the nodes
    net_kernel:connect_node(AcceptorNode),
    net_kernel:connect_node(ProposerNode),

    %% Start GUI on the control node (current node)
    AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
    AccRegister = [homer, marge, bart, lisa, maggie],
    ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
    PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),

    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            %% Start acceptors on acceptor node
            Begin = erlang:monotonic_time(),
            start_acceptors(AcceptorNode, AccIds, AccRegister),
            %% Start proposers on proposer node
            %print this address
            io:format("Main: ~p~n", [self()]),
            
            start_proposers(ProposerNode, PropIds, PropInfo, AccRegister, Sleep,self()),
            wait_proposers(length(PropIds)),
            End = erlang:monotonic_time(),
            Elapsed = erlang:convert_time_unit(End - Begin, native, millisecond),
            io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
            
    end.

    
start_acceptors(_Node, [], []) ->
    ok;
start_acceptors(Node, [AccId|RestIds], [RegName|RestNames]) ->
    spawn(Node, fun() ->
        register(RegName, acceptor:start(RegName, AccId))
    end),
    start_acceptors(Node, RestIds, RestNames).



start_proposers(Node,PropIds, PropInfo, Acceptors, Sleep, Main) ->
    case PropIds of
    [] ->
      ok;
    %% Adjust acceptors list to include node information
    [PropId|Rest] ->
    [{RegName, Colour}|RestInfo] = PropInfo,
    [FirstSleep|RestSleep] = Sleep,
    AcceptorsWithNodes = [{Name, 'paxy_acc@chomsky'} || Name <- Acceptors],
    spawn(Node, fun() ->
        originalproposer:start(RegName, Colour, AcceptorsWithNodes, FirstSleep, PropId, Main)
    end),
    start_proposers(Node, Rest, RestInfo, Acceptors, RestSleep, Main)
    end.


wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unregister(Name),
            exit(Pid, "crash"),
            pers:open(Name),
            {_, _, _, PanelId} = pers:read(Name),
            PanelId ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
            pers:close(Name),
            timer:sleep(2000),
            register(Name, acceptorFaultTolerance:start(Name, na))
    end.


stop() ->
    AcceptorNode = 'paxy_acc@chomsky',
    ProposerNode = 'paxy_pro@chomsky',
    %% Stop acceptors
    spawn(AcceptorNode, fun() ->
        stop_acceptors()
    end),
    %% Stop proposers
    spawn(ProposerNode, fun() ->
        stop_proposers()
    end),
    %% Stop GUI
    stop(gui).

stop_acceptors() ->
    AccRegister = [homer, marge, bart, lisa, maggie],
    [stop(Name) || Name <- AccRegister],
    ok.

stop_proposers() ->
    PropNames = [fry, bender, leela],
    [stop(Name) || Name <- PropNames],
    ok.

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
 
