%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Prolog Homework #2014 %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% 09 - 05 - 2014	%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%% Casper Thuis, 10341943, casper.thuis@hotmail.com   %%%%
%%%%%%%% Fije van Overeem, 10373535, fije@hotmail.com	%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%	Assignment 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% to do list %%%
% conflicts(vanuit concurrent kijkend).
% time line laten zien.
% "a before b" kan backtracken: misschien ergens een cut?
% Check bij het asserten van X before/after Y dat X en Y wel events zijn.
% Wel/niet check op het zijn van een event.
% 1000x concurrency failcheck
% We willen ipv legelijst-hackerssolution een proper reflectieve concurrencerelatie.

:- dynamic event/1.
:- dynamic before/2.
:- dynamic after/2.
:- dynamic concurrent/2.
:- dynamic beforeOrConcurrent/2.
:- dynamic afterOrConcurrent/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% defining operaters %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(700, xfy, before).
:- op(600, xfy, after).
:- op(500, xfy, concurrent).
:- op(400, xfy, beforeOrConcurrent).
:- op(400, xfy, afterOrConcurrent).

%['Fije.Overeem.Casper.Thuis#4.pl'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Assingment 1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showAllEvent:-
	findall([X],event(X), List),
	write(List).

showAllBefores:-
	findall([X,Y],X before Y, List),
	write(List).

showAllConcurrents:-
	findall([X,Y],X concurrent Y, List),
	write(List).

showAllAfters:-
	findall([X,Y],X after Y, List),
	write(List).

go1 :- 
	reset,
	assert(event(z)),
	assert(event(a)),
	assert(event(b)),
	assert(event(c)),
	assert(event(d)),
	assert(event(e)),
	assert(event(g)),
	assert(event(u)),
	assert(event(q)),
	assert(a before d),
	assert(a before b),
	assert(b before c),
	assert(c before d),
	assert(d before p),
	assert(g after y),
	assert(d concurrent y),
	assert(a concurrent e),
	assert(b concurrent q),
	assert(a concurrent i),
	assert(x concurrent a),	
	assert(c concurrent j),
	forward.


go2 :- 
	reset,
	assert(event(a)),
	assert(event(b)),
	assert(event(c)),
	assert(event(e)),
	assert(event(d)),
	assert(b before a),
	assert(c before a),
	%assert(b before d),
	assert(a concurrent e),
	%assert(d before e),
	forward.

forward :-
	 transitivity,
	 reflection,	
	 checkForIrregularities.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MakeTimeLines gives all the timeLines that are possible. 
%% does this by finding all relations that are not before and 
%% not concurrent whit that event. After this it put the event
%% in the list either in the next in the event or after the event.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeTimeLines:-
	setof(X, makeTimeLines(X), List),
	writeTimeLine(List).

writeTimeLine([]).

writeTimeLine([H|T]):-
	write(T),
	nl,
	writeTimeLine(T).

makeTimeLines(Output):-
	findall(Events, event(Events), EventList),
	makeTime(EventList, TempTimeLine),
	reverse(TempTimeLine,Output).


makeTime(EventList, Output):-
	findall(X, (X before _, not(_ before X)), FirstList),
	member(H, FirstList),
	select(H, EventList, NewEventList),
	append([[H]], [], NewTimeList),
	makeTimeLine(NewEventList, H,  NewTimeList, Output).

makeTimeLine([], _ ,X, X).

makeTimeLine(EventList, H, TimeList, Output):-
	findall(X, (member(X, EventList), not(X before H)), PossiblityList),
	member(G, PossiblityList),
	select(G, EventList, NewEventList),
	%putConcurrencesInList(G, NewEventList, NewerEventList, OutputCon),
	putEventInList(G, TimeList, NewTimeList),
	makeTimeLine(NewEventList, G, NewTimeList, Output).


putEventInList(X, [[H|T]|TimeList], [[H,X|T]|TimeList]):-
	not(H before X),
	X concurrent H, !.

putEventInList(X, [[H|T]|TimeList], [[X],[H|T]|TimeList]):-
	not(X concurrent H),
	H before X, !. 

putEventInList(X, [[H|T]|TimeList], NewEventList):-
	not(X concurrent H),
	not(H before X),
	((NewEventList = [[X,H|T]|TimeList]);
	(NewEventList = [[X],[H|T]|TimeList])),!.

putConcurrencesInList(X, EventList, NewEventList, Output2):-
	findall(Y, (member(Y, EventList), not(X after Y), not(Y after X)), PossibilityList),
	addConcurrences(PossibilityList, EventList, NewEventList, Output1),
	append([X], Output1, Output2). 

addConcurrences([], X, X, []).

addConcurrences([H|T], EventList, NewerEventList, [H|Output]):-
	select(H, EventList, NewEventList),
	addConcurrences(T, NewEventList, NewerEventList, Output).

/*
makeTimeline:-
	findall(X, (X before _, not(_ before X)), [H|_]),
	append([], [H], NewList),
	makeRestOfTimeline(H, NewList).

makeRestOfTimeline(H,TimeList):-
	not(H before _),
	writeList(TimeList).

makeRestOfTimeline(H, TimeList):-
	writeConcurrents(H, TimeList, NewTimeList),
	writeNextEvent(H, NewTimeList).

writeConcurrents(Y, TimeList, NewTimeList):-
	findall(X, Y concurrent X, List),
	append([Y], List, List2),
	list_to_set(List2, List3),
	select(Y, List3, SetList),
	append(SetList, TimeList, NewTimeList).

writeList([]):-
	write(')').

writeList([H|T]):-
	write(', '),
	write(H),
	writeList(T).

writeNextEvent(X, TimeList):-
	findall(Y, X before Y, List),
	findBestNextEvent(X, List, Z),
	append([Z], TimeList, NewTimeList),
	makeRestOfTimeline(Z, NewTimeList).

findBestNextEvent(X, [H|T], Z):-
	findall(Y, Y before H, BeforesList),
	((checkConcurrence(X, BeforesList), Z = H);
	(findBestNextEvent(X, T, Z))).

checkConcurrence(_, []).

checkConcurrence(X, [H|T]):-
	((X concurrent H);
	(H before X)),
	checkConcurrence(X, T).
	
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% retracts all the events and rules in the knowledge base %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset :-
	retractall(event(X)),
	retractall(X before Y),
	retractall(X after Y),
	retractall(X concurrent Y).	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate findalls the concurrent relations and add before
%% or after relations to from the first concurrent and add them
%% to the last concurrent. Thereby adding all the relations to
%% the other element making it reflectief.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inheritProperties:-
	setof([X, Y], is_concurrent(X, Y), TwinniesList),
	goThroughTwinniesList(TwinniesList).

goThroughTwinniesList([]).
	
goThroughTwinniesList([H|T]):-
	H = [X,Y],
	findall(Z, is_before(X, Z), BeforeList),
	assertAllBefores(Y, BeforeList),
	findall(Z, is_after(X, Z), AfterList),
	assertAllAfters(Y, AfterList),
	goThroughTwinniesList(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% is before    %%%%%%%%%%%%%%%%%%%%%%%%%%
%%this predicate checks whether a relations is transitief	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


is_before(X , Y) :-
	X before Y.

is_before(X , Z) :-
	X before Y,
	is_before(Y , Z).

is_after(X , Y) :-
	X after Y.

is_after(X , Z) :-
	X after Y,
	is_after(Y , Z).

is_concurrent(X, Y):-
	X concurrent Y;
	Y concurrent X.


is_concurrent(X, Z):-
	X concurrent Y,
	is_concurrent(Y, Z),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate activated all the transitif relations of all%
%% the events that are currently in the knowledge base. %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reflection:-
	changeAftersToBefores,
	changeBeforesToAfters,
	findAllConcurrents.

transitivity:-
	findall(X, event(X), EventsList),
	findAllBefores(EventsList),
	findAllAfters(EventsList),
	inheritProperties,
	changeAftersToBefores,
	changeBeforesToAfters.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate findsall the before relations of the all the%
%% events. It removes the duplicates and then sends it to a   %
%% assert function. It continues until the Eventlist is empty % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


findAllBefores([]).

findAllBefores([H|EventsList]) :-
	findall(Events, is_before(H , Events), List),
	list_to_set(List, EventList),
	assertAllBefores(H, EventList),
	findAllBefores(EventsList).

findAllAfters([]).

findAllAfters([H|EventsList]) :-
	findall(Events, is_after(H , Events), List),
	list_to_set(List, EventList),
	assertAllAfters(H, EventList),
	findAllAfters(EventsList).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate asserts all the possible combinations of	%%% 
%% the event, if the event already exist the fact is not 	%%%
%% asserted. it continues until no events are left 			%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assertAllBefores(_, []).

assertAllBefores(Event, [H|List]) :-
	\+ Event before H,
	assert(Event before H),
	assert(H after Event),
	assertAllBefores(Event, List).

assertAllBefores(Event, [_|List]) :-
	assertAllBefores(Event, List).

assertAllAfters(_, []).

assertAllAfters(Event, [H|List]) :-
	\+ Event after H,
	assert(Event after H),
	assert(H before Event),
	assertAllAfters(Event, List).

assertAllAfters(Event, [_|List]) :-
	assertAllAfters(Event, List).

	
findAllConcurrents:-
	findall([X , Y], X concurrent Y, ConcurrentList),
	addAllConcurrents(ConcurrentList).

addAllConcurrents([]).

addAllConcurrents([H|List]):-
	H = [X ,Y],
	\+ Y concurrent X,
	assert( Y concurrent X),
	addAllConcurrents(List).

addAllConcurrents([_|List]):-
	addAllConcurrents(List).

makeEventsReflective:-
	setof(X, ((X before _);(_ before X)), EventList),
	assertAllReflectiveConcurrents(EventList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate changes all before relations to after  	
%% relations by searching all the befores and adding the couter
%% example. The same is done for changeAllAfters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


changeBeforesToAfters:-
	findall([X, Y], X before Y, List),
	makeAfters(List).

makeAfters([]).

makeAfters([H|T]):-
	H = [X, Y],
	\+ Y after X,
	assert(Y after X),
	makeAfters(T).

makeAfters([_|T]):-
	makeAfters(T).

changeAftersToBefores:-
	findall([X, Y], X after Y, List),
	makeBefores(List).

makeBefores([]).

makeBefores([H|T]):-
	H = [X, Y],
	\+ Y before X,
	assert(Y before X),
	makeBefores(T).

makeBefores([_|T]):-
	makeBefores(T).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkforIrregulatities check if facts in the in the data 
%% interfer whit that current database.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

checkForIrregularities:-
	findall(X, event(X), EventsList),
	checkForCollision(EventsList).

checkForCollision([]).

checkForCollision([H|List]):-
	findall(Y, H before Y, BeforeList),
	checkBefores(H, BeforeList),
	findall(Y, H concurrent Y, ConcurrentList),
	checkConcurrent(H , ConcurrentList),
	checkForCollision(List).

checkBefores(_ , []).

checkBefores(Event, [H|List]):-
	\+ Event after H,
	\+ Event concurrent H,
	\+ H concurrent Event,
	checkBefores(Event, List).

checkBefores(_, _):-
	write('the facts you asserted are interfering with the database, probably a before or after wrong.'), fail.

checkConcurrent(_ , []).

checkConcurrent(Event, [H|List]):-
	\+ Event after H,
	\+ Event before H,
	checkConcurrent(Event, List).

checkConcurrent(_, _):-

	write('the facts you asserted are interfering with the database, probably a concurrent wrong.'), fail.
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% point predicate ask the user for input and add the facts one 
%% by one. It check for clashes whit current facts in the data-
%% base in the current database. If the fact clash the point 
%% it fails. if it doenst fail it add the fact to the database 
%% and adds the event if needs be.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

point:-
	write('Enter a point do you want a add before concurrent or after relation? '),
	readln(Input),
	assertList(Input),
	foward.

assertList([]).

assertList([H1,H2,H3|Rest]):-
	checkForClashes(H1,H2,H3),
	assertRelation(H1,H2,H3),
	assertEvents([H1,H3]),
	nl,
	assertList(Rest).

checkForClashes(H1,H2,H3):-
	H2 == before,
	not(H1 after H3),
	not(H1 concurrent H3),
	not(H3 concurrent H1),
	write('Relation'), write(' '),
	write(H1 ), write(' '),
	write(H2 ), write(' '),
	write(H3 ), write(' '),
	write('has been added'),
	nl.

checkForClashes(H1,H2,H3):-
	H2 == after,
	not(H1 before H3),
	not(H1 concurrent H3),
	not(H3 concurrent H3),
	write('Relation'), write(' '),
	write(H1 ), write(' '),
	write(H2 ), write(' '),
	write(H3 ), write(' '),
	write('has been added'),
	nl.
 

checkForClashes(H1,H2,H3):-
	H2 == concurrent,
	not(H1 before H3),
	not(H3 before H1),
	write('Relation'), write(' '),
	write(H1 ), write(' '),
	write(H2 ), write(' '),
	write(H3 ), write(' '),
	write('has been added'),
	nl.

assertRelation(H1,H2,H3):-
	((H2 == before,
	\+  H1 before H3,
	assert(H1 before H3));
	(H2 == after,
	\+ H1 after H3, 
	assert(H1 after H3));
	(H2 == concurrent,
	\+ H1 concurrent H3, 
	assert(H1 concurrent H3))).


assertEvents([]).

assertEvents([H|T]):-
	\+ event(H),
	assert(event(H)),
	write('Event asserted: '),
	write(H),
	nl,
	assertList(T).

assertEvents([H|T]):-
	assertEvents(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


