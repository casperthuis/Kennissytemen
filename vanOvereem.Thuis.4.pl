%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Prolog Homework #2014 %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% 09 - 05 - 2014	%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%% Casper Thuis, 10341943, casper.thuis@hotmail.com   %%%%
%%%%%%%% Fije van Overeem, 10373535, fije@hotmail.com	%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%	Assignment 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These are the main-predicates and operators the program needs. All dynamic, so that events and relations can be asserted and retracted when wanted. 

:- dynamic event/1.
:- dynamic before/2.
:- dynamic after/2.
:- dynamic concurrent/2.
:- dynamic beforeOrConcurrent/2.
:- dynamic afterOrConcurrent/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Defining operators %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(700, xfy, before).
:- op(600, xfy, after).
:- op(500, xfy, concurrent).
:- op(400, xfy, beforeOrConcurrent).
:- op(400, xfy, afterOrConcurrent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Assignment 1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These predicates are pretty convenient to check the programs.

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

forward :-

	 transitivity,
	 reflection,	
	 checkForIrregularities.


reset :- %retracts all the events and rules in the knowledge base

	retractall(event(X)), 
	retractall(X before Y),
	retractall(X after Y),
	retractall(X concurrent Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Examples of knowledge-bases, such that possible timelines can be formed. NB: An event % can only appear once in the timeline, unless you assert the event twice.

% In the representation of the timeline, when two or more events are in the same list 
% it means that they are happening at the same time (concurrent). When these eventlists % are seperated by an arrow, '--->', one event follows another.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N.B. It's not fully possible yet to use beforeOrConcurrent or afterOrConcurrent. We didn't have the time to implement that yet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% go1 describes all the possibles orders in which one can perform all the actions that % have to do with eating dinner. These are the timelines the user gets when asking for them with makePossibleTimelines:

/*

[1] 92 ?- makePossibleTimeLines.

[boodschappen_doen]--->[tafel_dekken,koken]--->[converseren,eten]--->[afwassen].
[boodschappen_doen]--->[tafel_dekken,koken]--->[eten,converseren]--->[afwassen].
[boodschappen_doen,tafel_dekken]--->[koken]--->[converseren,eten]--->[afwassen].
[boodschappen_doen,tafel_dekken]--->[koken]--->[eten,converseren]--->[afwassen].
[koken,tafel_dekken,boodschappen_doen]--->[converseren,eten]--->[afwassen].
[koken,tafel_dekken,boodschappen_doen]--->[eten,converseren]--->[afwassen].
_______________
true 

*/

% Please note that it gives 6 different timelines which are not correct. When looking at the facts, koken and booschappen_doen should not be able to concur. Also, it doesn't find all the combinations. On top of that it also seems as if the first two timelines are different timelines, when in reality they're the same, only 'converseren' and 'eten' are switched from place. 

% So, the timeline-creator doesn't quite do what it's supposed to yet. But it does find several right rimelines, and it also places all the events in the list, it doesn't leave out any. 

% With some knowledge-bases, it's not quite yet possible to create timelines because it will get stuck in an infinite loop somewhere. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


go1:- 

	reset, % reset retracts all sort of facts in the knowledgebase.

	assert(event(boodschappen_doen)),
	assert(event(koken)),
	assert(event(tafel_dekken)),
	assert(event(eten)),
	assert(event(afwassen)),
	assert(event(converseren)),
	
	assert(boodschappen_doen before koken),
	assert(tafel_dekken before eten),
	assert(eten before afwassen),
	assert(koken before eten),
	assert(converseren concurrent eten),

	forward. %makes all relations explicit and checks for inference.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* These are the possible timelines one gets from go2:



[1] 174 ?- makePossibleTimeLines.

[b]--->[a,d]--->[c].
[b]--->[d,a]--->[c].

_______________
true 

This is correct, although you could say that both timelines are actually the same.
*/

go2:- 

	reset,

	assert(event(a)),
	assert(event(b)),
	assert(event(c)),
	assert(event(d)),

	assert(b before a),
	assert(a before c),
	assert(a concurrent d),
	
	forward.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% makePossibleTimeLines writes out all the timelines that 'makeTimeLines(X)' can find. does this by finding all relations that are not before and 
%% not concurrent whit that event. After this it put the event
%% in the list either in the next in the event or after the event.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



makePossibleTimeLines:- 

	nl,
	setof(Timeline, makeTimeLines(Timeline), Timelines),
	writeOutTimelines(Timelines).

writeOutTimelines([]):-

	write('_______________').
writeOutTimelines([H|T]):-
	writeOutTimeline(H),nl,
	writeOutTimelines(T).

writeOutTimeline([]).

writeOutTimeline([H]):-

	write(H),
	write('.').

writeOutTimeline([H|T]):-

	write(H),
	write('--->'),
	writeOutTimeline(T).


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
	putEventInList(G, TimeList, NewTimeList),
	makeTimeLine(NewEventList, G, NewTimeList, Output).

putEventInList(X, [[H|T]|TimeList], [[H,X|T]|TimeList]):-

	not(H before X),
	X concurrent H, !.

putEventInList(X, [[H|T]|TimeList], [[X],[H|T]|TimeList]):-

	not(X concurrent H),
	H before X, !. 

putEventInList(X, [[H|T]|TimeList], NewEventList):-

	((NewEventList = [[X,H|T]|TimeList]);
	(NewEventList = [[X],[H|T]|TimeList])),!.









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
%%this predicate checks whether a relation is transitive   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


is_before(X , Y) :-

	X before Y.

is_before(X , Z) :-

	X before Y,
	is_before(Y , Z).

is_before(X, Y):-

	((X concurrent Z);
	(Z concurrent X)),
	Z before Y.

is_before(X, Y):-

	X before Z,
	((Z concurrent Y);
	(Y concurrent Z)).

is_after(X , Y) :-

	X after Y.

is_after(X , Z) :-

	X after Y,
	is_after(Y , Z).

is_after(X, Y):-

	((X concurrent Z);
	(Z concurrent X)),
	Z after Y.

is_after(X, Y):-

	X after Z,
	((Z concurrent Y);
	(Y concurrent Z)).


is_concurrent(X, Y):-

	X concurrent Y;
	Y concurrent X.

is_concurrent(X, Z):-

	X concurrent Y,
	is_concurrent(Y, Z),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate activated all the transitive relations of all%
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
%% This predicate finds all the before-relations of all the   %
%% events. It removes the duplicates and then sends it to a   %
%% assert function. It continues until the Eventlist is empty. % 
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
%% This predicate asserts all the possible combinations of  %%% 
%% the event, if the event already exist the fact is not    %%%
%% asserted. it continues until no events are left 	    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assertAllBefores(_, []).

assertAllBefores(Event, [H|List]) :-

	assert(Event before H),
	assert(H after Event),
	assertAllBefores(Event, List).

assertAllBefores(Event, [_|List]) :-

	assertAllBefores(Event, List).

assertAllAfters(_, []).

assertAllAfters(Event, [H|List]) :-

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
%% checkforIrregulatities check if facts in the data 
%% interfere with that current database.
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
%% and adds the event if needs be. To use point you can give a
%% list like this "a before b b before c c before d" whitout point or
%% comma's and then it will add all events.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

point:-

	write('Enter a point do you want a add before concurrent or after relation '), nl,
	write('To enter a relation you need to give a long line of relations:'), nl,
	write('for example input: "a before b b before c a after e" no point needed'),nl,
	write('If the function returns false your facts were not admissable or not understandable'),nl,
	readln(Input),
	assertList(Input),
	forward,
	makePossibleTimeLines.

assertList([]).

assertList([H1,H2,H3|Rest]):-

	checkForClashes(H1,H2,H3),
	assertRelation(H1,H2,H3),
	assertEvents([H1,H3]),
	nl,
	assertList(Rest).

checkForClashes(H1,H2,H3):-

	H2 == before,
	not(H3 before H1),
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

checkForClashes(_,_,_):-
	nl,
	write('Unfortunately the relations cannot be added to the knowledge base. Information is contradictory.').

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
	assertEvents(T).

assertEvents([_|T]):-

	assertEvents(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

go3/go4: intervals. 

We had an idea how to implement it, but didn't have the time to actually do it. We wanted to look at an interval as a time between two points.

You could say that at the interval X, which is from timepoint A to timepoint B, everything that happens in that interval is afterOrConcurrent A and beforeOrConcurrent B. 

This way relations between points and intervals can be defined and checked for irregularities.

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% go5 & go6: show what happens if contradictory
% We have a checkIrregularities predicate that almost works. Unfortunately we can not directly detect how to fix it, but we also have the checkForClashes/3 predicate which is used it the point/0 programme. This can also show if added content is contradictory:
/*
[1] 195 ?- go5.

Unfortunately the relations cannot be added to the knowledge base. Information is contradictory.
true.


[1] 198 ?- go6.

Unfortunately the relations cannot be added to the knowledge base. Information is contradictory.
true.

*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go5:-

	reset,

	assert(event(x)),
	assert(event(y)),
	assert(x before y),
	assert(y before x),
	
	checkForClashes(y, before, x).


go6:-
	
	reset,

	assert(event(x)),
	assert(event(y)),
	assert(x before y),
	assert(y concurrent x),
	
	checkForClashes(y, concurrent, x).





% We hope you can appreciate this program. -van Overeem & Thuis.












% Bunch of dead code 

/*
putConcurrencesInList(X, EventList, NewEventList, Output2):-
	findall(Y, (member(Y, EventList), not(X after Y), not(Y after X)), PossibilityList),
	addConcurrences(PossibilityList, EventList, NewEventList, Output1),
	append([X], Output1, Output2). 

addConcurrences([], X, X, []).

addConcurrences([H|T], EventList, NewerEventList, [H|Output]):-
	select(H, EventList, NewEventList),
	addConcurrences(T, NewEventList, NewerEventList, Output).
*/


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

	


