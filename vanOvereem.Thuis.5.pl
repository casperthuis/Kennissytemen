%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Prolog Homework #2014 %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% 18 - 05 - 2014	%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%% Casper Thuis, 10341943, casper.thuis@gmail.com %%%%%%%%
%%%%%%%% Fije van Overeem, 10373535, fije@hotmail.com	%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%	Assignment 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%je houdt bij wat de aannames zijn bij een bepaalde berekening. 

showAllInputs:-
	findall([X,Y], input(X,Y), List),
	write_ln(List).

showAllOutput:-
	findall([X,Y], expectedOutput(X,Y), List),
	write_ln(List).

showAllCompnents:-
	findall([X,Y,Q,Z], component(X,Y,Q,Z), List),
	write_ln(List).

:- dynamic input/2.
:- dynamic output/2.
:- dynamic component/4.

reset :-
	retractall(input(_,_)),
	retractall(output(_,_)),
	retractall(component(_,_,_,_)).

go1:-

	reset,

	assert( input(a, 3)),
	assert( input(b, 2)),
	assert( input(c, 2)),
	assert( input(d, 3)),
	assert( input(e, 3)),

	assert( component(m1, multi, [a, c], x)),
	assert( component(m2, multi, [b, d], y)),
	assert( component(m3, multi, [c, e], z)),
	assert( component(a1, adder, [x, y], f)),
	assert( component(a2, adder, [y, z], g)).



go2:-

	reset,

	assert( input(a, 3)),
	assert( input(b, 2)),
	assert( input(c, 2)),
	assert( input(d, 3)),
	assert( input(e, 3)),
	assert( input(f, 2)),
	assert( input(g, 3)),

	assert( component(a1, adder, [a, b], h)),
	assert( component(m1, multi, [c, d], i)),
	assert( component(m2, multi, [h, i], j)),
	assert( component(a2, adder, [j, e], k)),
	assert( component(m3, multi, [j, f], l)),
	assert( component(m4, multi, [k, e], m)),
	assert( component(m5, multi, [l, e], n)),
	assert( component(a3, adder, [m, g], o)),
	assert( component(m6, multi, [n, g], p)),
	assert( component(a4, adder, [o, p], q)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



forward:-
	findall([X,Y], component(_, _, [X, Y], _), InputList),
	findNextStep(InputList).

findNextStep([]).

findNextStep([H|Rest]):-
	H = [Input1, Input2],
	component(_, Sort, [Input1,Input2], OutputName),
	getValueOfComponent(Sort, [Input1,Input2], Value),
	%not( output(OutputName, Value) ),
	assert( expectedOutput(OutputName, Value) ),
	write( 'Derived:' ), write_ln( expectedOutput(OutputName, Value)),
	checkIfOutputIsInput(OutputName, Value ),
    findNextStep(Rest).

findNextStep([H|Rest]):-
	append(Rest, H, NewRest),
	findNextStep(NewRest).		

getValueOfComponent(Sort, [Input1, Input2], Output):-
	(Sort == 'multi',
	input(Input1, Value1 ),
	input(Input2, Value2 ),	
	Output is Value1 * Value2);
	(Sort == 'adder',
	input(Input1, Value1 ),
	input(Input2, Value2 ), 
	Output is Value1 + Value2).

%nog ff kijken voor een regel voor input bijvoorbeeld als 1 aan het eind van een component en als input voor een component is.
checkIfOutputIsInput(OutputName, Value):-
	findall(Inputs, component(_,_,Inputs,_), InputsList),
	flatten(InputsList, FlattenList),
	member(OutputName, FlattenList),
	not( input( OutputName, Value )),
	assert( input( OutputName, Value )).

checkIfOutputIsInput(_,_).



backward(Output):-
	findall(Y, (expectedOutput(Y, Output), not( input(Y, Output))), PossibleList),
	member(X, PossibleList),
	findPreviousStep(X).

findPreviousStep(Output):-
	findall(X, (input(X, _), not( expectedOutput(X, _) ) ), AnswerList),
	member(Output, AnswerList).

findPreviousStep(Output):-
	component(_, _, [Input1, Input2], Output),
	findPreviousStep(Input1),
	findPreviousStep(Input2).
	
/*
is_true( P ):-
    fact( P ).

is_true( P ):-
    if Condition then P,
    is_true( Condition ).

is_true( P1 and P2 ):-
    is_true( P1 ),
    is_true( P2 ).

is_true( P1 or P2 ):-
    is_true( P1 )
    ;
    is_true( P2 ).

*/


		

/*
forward:-
    new_derived_fact( P ),
    !,
    write( 'Derived:' ), write_ln( P ),
    assert( fact( P )),
    forward
    ;
    write_ln( 'No more facts' ).

new_derived_fact( Conclusion ):-
    if Condition then Conclusion,
    not( fact( Conclusion ) ),
    composed_fact( Condition ).

composed_fact( Condition ):-
    fact( Condition ).

composed_fact( Condition1 and Condition2 ):-
    composed_fact( Condition1 ),
    composed_fact( Condition2 ).

composed_fact( Condition1 or Condition2 ):-
    composed_fact( Condition1 )
    ;
    composed_fact( Condition2 ).

*/
/* --- A simple backward chaining rule interpreter --- */
/*
is_true( P ):-
    fact( P ).

is_true( P ):-
    if Condition then P,
    is_true( Condition ).

is_true( P1 and P2 ):-
    is_true( P1 ),
    is_true( P2 ).

is_true( P1 or P2 ):-
    is_true( P1 )
    ;
    is_true( P2 ).

*/
/* --- A simple forward chaining rule interpreter --- */

