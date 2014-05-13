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
	findall([X,Y], output(X,Y), List),
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

	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



forward:-
	findall([X,Y], component(_, _, [X, Y], _), InputList),
	findNextStep(InputList).

findNextStep([]).

findNextStep([H|Rest]):-
	H = [Input1, Input2],
	component(Name, Sort, [Input1,Input2], OutputName),
	getValueOfComponent(Sort, [Input1,Input2], Value),
	not( output(OutputName, Value) ),
	assert( output(OutputName, Value) ),
	write( 'Derived:' ), write_ln( Name ),
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

checkIfOutputIsInput(OutputName, Value):-
	not( input( OutputName, Value )),
	assert( input( OutputName, Value )).

checkIfOutputIsInput(_,_).


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

