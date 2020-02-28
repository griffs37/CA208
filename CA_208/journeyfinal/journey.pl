/* 
  Sample set of facts 
*/
route(dublin, cork, 200, 'fct').
route(cork, dublin, 200, 'fct').
route(cork, corkAirport, 20, 'fc').
route(corkAirport, cork, 25, 'fc').
route(dublin, dublinAirport, 10, 'fc').
route(dublinAirport, dublin, 20, 'fc').
route(dublinAirport, corkAirport, 225, 'p').
route(corkAirport, dublinAirport, 225, 'p').
  
/* 
  Speed of mode of transport used
*/
speed(f, 5).
speed(c, 80).
speed(t, 100).
speed(p, 500). 
 
/* 
  Checks if (Source to Destination) can be travelled via String Mode 
*/
travel_Modes(Modes, X, R) :-
  travel_Modes_rec(Modes, X, [], R).
 
/* 
  travel_Modes validates / checks if Mode is in String turned Char
*/
travel_Modes_rec(Modes, X, A, Res) :-
  route(X, N, _, TravelRoute),
  atom_chars(Modes, RouteList),
  atom_chars(TravelRoute, TravelRouteList),
  intersection(RouteList, TravelRouteList, [_|_]),
  \+ member(N, A), !,
  travel_Modes_rec(Modes, X, [N|A], Res).
travel_Modes_rec(_, _, A, A).
 
/* 
  time predicate which calculates time
*/
time(Modes, From, To, Time) :-
  atom_chars(Modes, RouteList),
  route(From, To, Distance, TravelRoute),
  atom_chars(TravelRoute, TravelRouteList),
  intersection(RouteList, TravelRouteList, CommonModes),
  best_mode_in_list(CommonModes, BestMode),
  speed(BestMode, Speed),
  Time is Distance / Speed,
  writeln(Time).

/*
  best_mode predicate selects the BestMode to use for a route
  to travel from Source to Destination by comparing the speeds
  of different modes to one another
*/
best_mode_in_list([Mode|Modes], BestMode):-
  best_mode_rec(Modes, Mode, BestMode).

best_mode_rec([Mode|Modes], TempMode, BestMode):-
  best_mode(Mode, TempMode, AnotherTempMode),
  best_mode_rec(Modes, AnotherTempMode, BestMode).

best_mode_rec([], BestMode, BestMode).
 
best_mode(Mode1,Mode2,Mode1):-
  speed(Mode1,Speed1),
  speed(Mode2,Speed2),
  Speed1 > Speed2, !.
best_mode(_,Mode,Mode).

/* 
  timeQueue predicate stores time values in list or necessary node
  to be used when called by route generation predicate
*/ 
timeQueue(Node, [[Node, _, Time]|_], Time):- !.

timeQueue(Node, [_|T], Time):-
  timeQueue(Node, T, Time).

timeQueue(_, [], 0).


/* 
  ogNode predicate stores original nodes (aka the parent node)
  checks if parent node has exiting node
*/ 
ogNode(_, [], nil).

ogNode(Node, [[Node, Parent, _]|_], Parent) :- !.

ogNode(Node, [_|T], Parent) :-
  ogNode(Node, T, Parent).
 
queue_has_node([[Node,_,_]|_],Node) :- !.

queue_has_node([_|T],Node) :-
  queue_has_node(T,Node).

/* 
  insertNode predicate inserts a node if time is faster (aka lower time is better)
*/
insertNode(Node, Parent, Time, [Head|Tail], [Head|Tail1]) :-
  Head = [_, _, Time1],
  Time > Time1, !,
  insertNode(Node, Parent, Time, Tail, Tail1).
insertNode(Node, Parent, Time, [Head|Tail], [[Node, Parent, Time], Head|Tail]) :-
  Head = [_, _, Time1],
  Time =< Time1, !.
insertNode(Node, Parent, Time, [], [[Node, Parent, Time]]).


/* 
  deleteNode predicate deletes a node (Not all test cases will work)
  in event of conflict / overwriting time
*/ 
deleteNode(Node, [[Node, _, _]|Tail], Tail) :- !.

deleteNode(Node, [Head0|Tail0], [Head0|Tail1]) :-
  deleteNode(Node, Tail0, Tail1).


/* 
  Implementation of Dijkstra Algorithm
*/
dijkstra(Node, Goal, Modes, Queue, CompletedList, Res):-
  Node = [NodeName,_,_],
  NodeName \= Goal,
  travel_Modes(Modes, NodeName, Neighbors),
  \+ queue_has_node(CompletedList, NodeName),
  updateNode(Node,Modes, Neighbors, CompletedList, Queue, Queue1), !,
  Queue1 = [Next|Queue2],
  dijkstra(Next, Goal, Modes, Queue2, [Node|CompletedList], Res).
dijkstra(Node, Goal, _, _, CompletedList, Res):-
  Node = [NodeName, _, _],
  NodeName = Goal,
  reverse([Node|CompletedList], Res).
 
/* 
  updateNode predicate updates status of node (if traversed, finished traversing, not yet)
  1) Node not yet traversed - Checks if it suits the conditions to be inserted (path existing, etc)
  2) Node in queue - checks and compares time values of different available travelMode.
  If different travelMode exists with faster time, old one is deleted and new one is added (basically updated)
  3) Node in queue - Will then be traversed / inserted
*/
updateNode(_, _, [], _, Queue, Queue).
updateNode(ParentNode, Modes, [Node|Nodes], CompletedList, QueueIn, QueueOut):-
  queue_has_node(CompletedList, Node), !,
  updateNode(ParentNode, Modes, Nodes, CompletedList, QueueIn, QueueOut).
updateNode(ParentNode, Modes, [Node|Nodes], CompletedList, QueueIn, QueueOut):-
  % Node not in CompletedList
  ParentNode = [ParentName, _, ParentTime],
  time(Modes, ParentName, Node, NodeParentRouteTime),
  NodeTime is ParentTime + NodeParentRouteTime,
  updateNode_operation(Modes, ParentName, Node,NodeTime, QueueIn, Queue1),
  updateNode(ParentNode, Modes, Nodes, CompletedList, Queue1, QueueOut).
 
/* 
  updateNode_operation - performs the operations above ^
*/ 
updateNode_operation(_,Parent,Node,NodeTime,QueueIn,QueueOut):-
  queue_has_node(QueueIn,Node), !,
  timeQueue(Node,QueueIn,OldTime),
  ( NodeTime < OldTime
  ->
    deleteNode(Node,QueueIn,Queue1),
    insertNode(Node,Parent,NodeTime,Queue1,QueueOut)
  ; QueueOut = QueueIn ).

updateNode_operation(_,Parent,Node,NodeTime,QueueIn,QueueOut):-
  insertNode(Node,Parent,NodeTime,QueueIn,QueueOut),
  write("Total Time: "), write(NodeTime), nl.

/* 
  Generate Complete Path From Source to Destination by loading CompletedList 
*/ 
generate(From, To, Nodes, Path) :- generatePath(From, To, Nodes, RPath), reverse(RPath, Path).
generatePath(N, N, _, [N]):- !.
generatePath(From, To, Nodes, [To|Path]):-
  ogNode(To, Nodes, Prev),
  generatePath(From, Prev, Nodes, Path).


/* 
  Journey predicate - takes user input of format: journey(S, D, M) and outputs time taken for the fastest route
  and generates the fastest route
*/ 
journey(Source, Destination, Mode) :-
  dijkstra([Source, nil, 0],Destination ,Mode ,[] ,[] , R), 
  generate(Source, Destination, R, Path),
  write("Shortest Path from "), write(Source), write(" to "), write(Destination), write(" is: "), nl,
  writeln(Path).