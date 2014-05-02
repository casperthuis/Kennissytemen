% INFO

:- write('Welcome to the animal knowledge base!').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic predicates
%
% Make the appropriate predicates dynamic to enable user to add
% concepts, relations and attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic concept/1, isA/2, attribute/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Concepts
%
% All the concepts in the knowledge base
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concept(thing).

concept(animal).
concept(individual).
concept(cover).
concept(movement_mechanism).

concept(legs).

concept(vertibrate).
concept(arthropod).

concept(bird).
concept(mammal).
concept(fish).
concept(insect).

concept(fur).
concept(skin).
concept(feathers).
concept(scales).
concept(other).

concept(owl).
concept(orca).
concept(panther).
concept(salmon).
concept(human).
concept(cockroach).
concept(spider).
concept(ant).
concept(amoeba).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subsets
%
% List of all subsets/ 'is a' relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isA(animal, thing).
isA(limbs, thing).
isA(cover, thing).
isA(individual, thing).

isA(vertebrate, animal).
isA(arthropod, animal).
isA(protista, animal).

isA(bird, vertebrate).
isA(mammal, vertebrate).
isA(fish, vertebrate).
isA(spider, arthropod).
isA(cockroach, insect).
isA(ant, insect).
isA(insect, arthropod).

isA(amoeba, protista).
isA(owl, bird).
isA(orca, mammal).
isA(human, mammal).
isA(panther, mammal).
isA(salmon, fish).
isA(cockroach, arthropod).
isA(spider, arthropod).
isA(amoeba, protista).

isA(fur, cover).
isA(skin, cover).
isA(feathers, cover).
isA(scales, cover).

isA(movement_mechanism, thing).
isA(legs, movements_mechanism).
isA(fins, movements_mechanism).
isA(wings, movements_mechanism).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attribute(animal, cover, covered, nil).

attribute(panther, fur, covered, nil).
attribute(human, skin, covered, nil).
attribute(orca, skin, covered, nil).

attribute(bird, feathers, covered, nil).
attribute(fish, scales, covered, nil).
attribute(human, skin, covered, nil).
attribute(orca, skin, covered, nil).
attribute(cockroach, other, covered, nil).
attribute(spider, other, covered, nil).
attribute(ant, other, covered, nil).
attribute(amoeba, other, covered, nil).

attribute(animal, movement_mechanism, has, nil).
attribute(owl, legs, has, 2).
attribute(human, legs, has, 2).
attribute(panther, legs, has, 4).
attribute(fish, fins, has, 7).
attribute(bird, legs, has, 2).
attribute(bird, wings, has, 2).

attribute(orca, fins, has, 7).
attribute(insect, legs, has, 6).
attribute(amoeba, legs, has, 0).
attribute(spider, legs, has, 8).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inherite attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attribute(X, A, B, C) :-
	isA(X, Y), attribute(Y, A, B, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check is a
%
% Checks for an 'is a' relation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkIsA(X, Y) :- isA(X,Y).

checkIsA(X, Y) :-
	isA(X, Z),
	checkIsA(Z, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check is an attribute
%
% Checks if something is an attribute of something else
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkAttribute(X, A, B, C) :-
	attribute(X, A, B, C).

checkAttribute(X, A, B, C) :-
	checkIsA(X, Z),
	checkAttribute(Z, A, B, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show :-
	findall(Concept, concept(Concept), List),
	writeConcepts(List). 

show(Concept) :-
	writeConcepts([Concept]). 

writeConcepts([H|T]) :-
	write('------------------------------  '), nl,
	write(H), nl,
    write('------------------------------  '), nl,
	showWhatItIs(H),
	showAttributes(H), nl,
	writeConcepts(T).

showWhatItIs(Concept) :-
	findall((Concept, Y), isA(Concept, Y), List),
	removeDup(List, CleanList),
	writeWhatItIs(CleanList).

writeWhatItIs([]) :-
	 nl.

writeWhatItIs([(_, Y)|T]) :-
	write(' Is a(n): '),
	write(Y),  
	nl,
	writeWhatItIs(T).

showAttributes(Concept) :-
	findall((Concept, A, B, C), attribute(Concept, A, B, C), List),
	removeDup(List, CleanList),
	write('Attributes:'), nl,
	writeAttributes(CleanList).

writeAttributes([]) :-
	 nl.

writeAttributes([H|T]) :-
	H = (_, A, B, C),
	write(' '),
	write(B), 
	write(': '),
	write(A),
	write(' (n/r: '),
	write(C),  
	write(')'),
	nl,
	writeAttributes(T).

removeDup([], []).

removeDup([First | Rest], NewRest) :-
	member(First, Rest),
	removeDup(Rest, NewRest).

removeDup([First | Rest], [First | NewRest]) :-
	not(member(First, Rest)),
	removeDup(Rest, NewRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add and retract
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addConcept(Concept, Superclass) :-
	\+ isA(Superclass, individual),
	assert(concept(Concept)),
	assert(isA(Concept, Superclass)).

addAttribute(Concept, A, B, C) :-
	assert(attribute(Concept, A, B, C)).

addIsA(Concept, Superclass) :-
	\+ isA(Superclass, individual),
	assert(isA(Concept, Superclass)).

clear :-
	retractall(concept(_)),
	retractall(isA(_, _)),
	retractall(attribute(_, _, _, _)),
	write('Knowledge base cleared.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Go
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go1 :- 
	addConcept(plant, thing),
	write('The fully new concept "plant" is added to the knowledge base (a).').

go2 :- 
	addConcept(human, mammal),
	addConcept(opposable_thumbs, thing),
	addAttribute(human, opposable_thumbs, has, 2),
    addAttribute(human, arms, has, 2),
    addAttribute(human, legs, has, 2),
	write('The fully subsumed concept "human" is added to the knowledge base (b).').

go3 :- 
	addConcept(lionfish, fish),
	write('The concept with missing attributes/values (covered: scales) "lionfish" is added to the knowledge base (c).').
	
go4 :- 
	write('5 new animals are added to the knowledge base: .').

go5 :- 
	addConcept(albert_the_space_monkey, mammal),
	addIsA(albert_the_space_monkey, individual),
    addAttribute(albert_the_space_monkey, legs, has, 4),
	write('The individual "albert_the_space_monkey" is added to the knowledge base. (it\'s impossible to add subclasses to individuals (w/ addIsA))').	
