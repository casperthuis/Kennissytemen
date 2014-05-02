%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Prolog Homework #2014 %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% 12 - 02 - 2014	%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%% Casper Thuis, 10341943, casper.thuis@hotmail.com	%%%%
%%%%%%%% Fije van Overeem, 10373535, fije@hotmail.com	%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%	Assingment 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% to do list %%%
% conflicts(vanuit concurrent kijkend).
% time line laten zien.
% "a before b" kan backtracken: misschien ergens een cut?
% Check bij het asserten van X before/after Y dat X en Y wel events zijn.

:- dynamic event/1.
:- dynamic before/0.
:- dynamic after/0.
:- dynamic concurrent/0.
:- dynamic beforeOrConcurrent/0.
:- dynamic afterOrConcurrent/0.

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
%%%%%%%%% time line should look like a ---> b ---> c %%%%%%%%%%
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
	assert(event(z)),
	assert(event(a)),
	assert(event(b)),
	assert(event(c)),
	assert(event(d)),
	assert(event(e)),
	assert(event(g)),
	%assert(z before a),
	assert(a before b),
	assert(b before c),
	assert(c before d),
	assert(g after y),
	assert(a concurrent e),
	assert(a concurrent f),
	assert(a concurrent u),
	assert(a concurrent i),
	assert(x concurrent a),	
	forward.


go2 :- 
	assert(event(a)),
	assert(event(b)),
	assert(event(c)),
	assert(a before b),
	assert(b before c),
	forward.



forward :-
	 transitivity,
	 reflection,	
	 checkForInregularities.

makeTimeline:-
	findall(X, (X before _, not(_ before X)), [H|_]),
	write(H),
	writeConcurrents(H).

writeConcurrents(Y):-
	findall(X, Y concurrent X, List),
	writeList(List).

writeList([]).

writeList([H|T]):-
	write(', '),
	write(H),
	writeList(T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% retracts all the events and rules in the knowledge base %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset :-
	retractall(event(X)),
	retractall(X before Y),
	retractall(X after Y),
	retractall(X concurrent Y).	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inheritProperties:-
	findall([X, Y], is_concurrent(X, Y), TwinniesList),
	goThroughTwinniesList(TwinniesList).

goThroughTwinniesList([]).
	
goThroughTwinniesList([H|T]):-
	H = [X,Y],
	findall(Z, X before Z, BeforeList),
	assertAllBefores(Y, BeforeList),
	findall(Z, X after Z, AfterList),
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
	is_concurrent(Y, Z).


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
	inheritProperties.

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
	assertAllBefores(Event, List).

assertAllBefores(Event, [_|List]) :-
	assertAllBefores(Event, List).

assertAllAfters(_, []).

assertAllAfters(Event, [H|List]) :-
	\+ Event after H,
	assert(Event after H),
	assertAllAfters(Event, List).

assertAllAfters(Event, [_|List]) :-
	assertAllAfters(Event, List).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate changes all before relations to after 	%%%
%% to after relations										%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


checkForInregularities:-
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
	write('the fact you asserted are interferring whit the database, probably a before or after wrong.'), fail.

checkConcurrent(_ , []).

checkConcurrent(Event, [H|List]):-
	\+ Event after H,
	\+ Event before H,
	checkConcurrent(Event, List).

checkConcurrent(_, _):-

	write('the fact you asserted are interferring whit the database, probably a concurrent wrong.'), fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




