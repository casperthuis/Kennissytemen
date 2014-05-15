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
:- dynamic input/2.
:- dynamic output/2.
:- dynamic component/4.

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
	assert( component(a2, adder, [y, z], g)),
	assert( measuredOutput(f, 10)),
	assert( measuredOutput(g, 12)),
	assert( measuredInput(a, 3)),
	assert( measuredInput(b, 2)),
	assert( measuredInput(c, 2)),
	assert( measuredInput(d, 3)),
	assert( measuredInput(e, 3)).



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

multiply([X,Y],Z):-
	Z = X * Y,
	component(_, _, [X,Y], Z).


forward:-
	findall([X,Y], component(_, _, [X, Y], _), InputList),
	findNextStep(InputList).

findNextStep([]).

findNextStep([H|Rest]):-
	H = [Input1, Input2],
	component(_, Sort, [Input1,Input2], OutputName),
	getValueOfComponent(Sort, [Input1,Input2], Value),
	assert( expectedOutput(OutputName, Value) ),
	write( 'Derived: ' ), write_ln( expectedOutput(OutputName, Value)),
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
	findall(Inputs, component(_,_,Inputs,_), InputsList),
	flatten(InputsList, FlattenList),
	member(OutputName, FlattenList),
	not( input( OutputName, Value )),
	assert( input( OutputName, Value )).

checkIfOutputIsInput(_,_).


findFaultNodes(FaultList):-
	findall([Name,Value], measuredOutput(Name, Value), MeasuredList),
	findTheWrongValues(MeasuredList, FaultList).

findTheWrongValues([],[]).

findTheWrongValues([First|MeasuredList], [First|FaultList]):-
	First = [Name, Value1],
	expectedOutput(Name, Value2),
	not( Value1 == Value2),
	findTheWrongValues(MeasuredList, FaultList).

findTheWrongValues([_|MeasuredList], FaultList):-
	findTheWrongValues(MeasuredList, FaultList).

findConflictsSet(FaultNodes , Output):-
	member(X, FaultNodes),
	X = [Name, _ ],
	List = [],
	backward(Name , List, Conflictset),
	list_to_set(Conflictset, ReverseList),
	reverse(ReverseList, Output).

backward(Name, List, Output):-
	findall(Inputs, measuredInput(Inputs, _), AnswerList),
	member(Name, AnswerList),
	List = Output.
	
backward(Name, List, Output2):-
	component(ComponentName, _, [Input1, Input2], Name),
	backward(Input1, [ComponentName|List], Output1),
	backward(Input2, [ComponentName|Output1], Output2).


backward2(Name, List):-
	findall(Inputs, measuredInput(Inputs, _), AnswerList),
	member(Name, AnswerList).

backward2(Name, List):-
	findall(ComponentNames, component(ComponentNames, _, _, Name), ComponentNameList),
	component(_, _, [Input1, Input2],Name),
	backward2(Input1, [ComponentNameList|List]),
	backward2(Input2, [ComponentNameList|List]). 

addNewMeasuredInput(List):-
	findall([X,Y], (component(X,_,[Input1,Input2],_), member(X, List),(measuredInput(Input1, _ ), InputList)),
	forwardExpected().


%%backward chaining geeft de mogelijke componenten die kapot zijn.!
/*
backward(Name):-
	findall(Y, (expectedOutput(Y, Output), not( input(Y, Output))), PossibleList),
	member(X, PossibleList),
	findPreviousStep(X).

findPreviousStep(Name, List)
	findall(Inputs, measuredInput(Inputs, _), AnswerList),
	member(Name, AnswerList).

findPreviousStep(Name, List):-
	component(ComponentName, _, [Input1, Input2], Name),
	append([ComponentName], List, NewList),
	findPreviousStep(Input1, NewList),
	append([ComponentName], NewList, NewerList),
	findPreviousStep(Input2, NewerList).


findPreviousStep(Output):-
	findall(X, (input(X, _), not( expectedOutput(X, _) ) ), AnswerList),
	member(Output, AnswerList).

findPreviousStep(Output):-
	component(_, _, [Input1, Input2], Output),
	findPreviousStep(Input1),
	findPreviousStep(Input2).
*/	

%% conflict herkenning
%3 optie of a1 of m2 of m1 of m1 en m2. 
