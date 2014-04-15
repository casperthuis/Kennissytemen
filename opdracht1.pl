%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Homework 1# kennis systemen Names: Daniel Staal, Casper Thuis%%%
%% StudentNumbers: , 10341943 									%%%
%% Email: daniel.staal@hotmail.com, casper.thuis@gmail.com  	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic fact/3.
:- dynamic fact/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Assingment 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All the "is a" relations are put into a "isa" predicate. All the%
% has a relations are put into a has a relation. There are 2 kind %
% of has predicates one whit 3 arguments and one whit 4 arguments.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isa2(X, Z):-
	isa(X, Y),
	isa(Y, Z).

isa(dier, thing).
isa(ledematen, thing).
isa(huid, thing).
isa(bloedig, thing).
isa(voortplanting, thing).
isa(vliegen, thing).

isa(zoogdier, dier).
isa(vis, dier).
isa(insect, dier).
isa(vogel, dier).

isa(goudvis, vis).
isa(kever, insect).
isa(spin, insect).
isa(aap, zoogdier).
isa(kat, zoogdier).

%pinguin
%haai
%kanarie
%sprinkhaan
%beer

has(poten, zoogdier, 4/4).
has(zoogt, zoogdier, 1/1).
has(vacht, zoogdier, 1/1).
has(warmbloedig, zoogdier, 1/1).

has(koudbloedig, vis, 1/1).
has(eieren, vis, 1/1).
has(schubben, vis, 1/1).

has(vleugels, vogel, 2/2).
has(warmbloedig, vogel, 1/1).
has(eieren, vogel, 1/1).
has(poten, vogel, 2/2).
has(veren, vogel, 1/1).

has(poten, insect, 6/8).
has(eieren, insect, 1/1).
has(vleugels, insect, 0/2).
has(koudbloedig, insect, 1/1).

has(bananen, aap, 1/1).
has(snorharen, kat, 1/1).
has(kom, goudvis, 1/1).
has(zwart, kever, 1/1).
has(web, spin, 1/1).


/*
rule(has, X, Z, _):-
	isa, X, Y),
	has, Y, Z, _).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Assingment 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Show predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The show predicate consist of 2 parts, first the findall part 	%
% finds all the Is a relations the concept has, after which it 	  	%
% we use a writeIS function to writes down what is is. After That  	%
% anothor findall function find all has a relations the concept has.% 
% After that a WriteHas function writes down all those relations 	%
% Then all the properties of a concept are writen down the concept 	%
% goes one higher up the tree, And recursively repeats it self until%
% the basecase of thing is reached.									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show(thing):- !.


show(DIER):-
	findall(DIER/SUPERKLASSE, isa(DIER, SUPERKLASSE), BAG),
	writeIs(BAG),
	findall(EIGENSCHAP/DIER/N, has(EIGENSCHAP,DIER, N), BAG2),
	writeAllProperties(BAG2),

	isa(DIER, DIER2),
	show(DIER2).

writeAllProperties([]).

writeAllProperties([H|T]):-
	writeHas(H),
	writeAllProperties(T).

writeHas(EIGENSCHAP/DIER/(GETAL1/_)):-
	write( 'Een '),
	write(DIER),
	write(' Heeft '),
	write(GETAL1),
	write(' '),
	write(EIGENSCHAP),
	nl.



writeIs([H1/H2]):-
	write('Een '),
	write(H1 ),
	write(' is een '),
	write(H2),
	nl.












/*
go1:-
	assert(isa, kanarie, vogel)),
	assert(has, gelekleur, kanarie, 1/1)).

go2:-
	assert(isa, plant, thing)).

go3:-
	assert(isa, plant, thing)).

go4:-
	assert(isa, plant, thing)).

go5:-
	assert(isa, plant, thing)).
*/

