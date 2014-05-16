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
:- dynamic goodset/2.
:- dynamic measuredInput/2.
:- dynamic measuredOutput/2.
:- dynamic expectedInput/2.
:- dynamic expectedOutput/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%je houdt bij wat de aannames zijn bij een bepaalde berekening. 

showAllInputs:-

	findall([X,Y], input(X,Y), List),
	write_ln(List).

showAllOutput:-

	findall([X,Y], expectedOutput(X,Y), List),
	write_ln(List).

showAllComponents:-

	findall([X,Y,Q,Z], component(X,Y,Q,Z), List),
	write_ln(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset :-

	retractall(input(_,_)),
	retractall(output(_,_)),
	retractall(component(_,_,_,_)),
	retractall(measuredInput(_,_)),
	retractall(expectedInput(_,_)),
	retractall(expectedOutput(_,_)),
	retractall(measuredOutput(_,_)),
	retractall(goodset(_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	assert( measuredOutput(g, 10)),
	%assert( measuredInput(y, 6)),
	%assert( measuredInput(x, 6)),
	assert( measuredInput(a, 3)),
	assert( measuredInput(c, 2)).
	%assert( measuredInput(c, 2)),
	%assert( measuredInput(d, 3)),
	%assert( measuredInput(e, 3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go2:-

	reset,

	assert( input(a, 3)),
	assert( input(b, 2)),
	assert( input(c, 2)),
	assert( input(d, 3)),
	assert( input(e, 3)),
	assert( input(f, 2)),
	assert( input(g, 3)),

	assert( measuredOutput(q, 642)),
	assert( measuredInput(a, 3)),
	assert( measuredInput(b, 2)),
	assert( measuredInput(c, 2)),
	assert( measuredInput(d, 3)),
	assert( measuredInput(e, 3)),
	assert( measuredInput(f, 2)),
	assert( measuredInput(g, 3)),

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



go3:-

	assert(input(a, 1)),
	assert(input(b, 1)),
	assert(measuredOutput(c, 1)),
	assert(component(a1, adder, [a, b], c)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


makeGrid:-

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Hier even een voorbeeldje schrijven van hoe dit gaat.

getValueOfOther(ComponentSort, OutputValue, ExpectedInputValue, OtherInputValue):-

	(ComponentSort == multi, backwardMulti(OutputValue, ExpectedInputValue, OtherInputValue);
	ComponentSort == adder, backwardAdder(OutputValue, ExpectedInputValue, OtherInputValue)), !.

backwardMulti(Value, ExpectedValue , Output):-

	Output is Value / ExpectedValue.

backwardAdder(Value, ExpectedValue , Output):-

	Output is Value - ExpectedValue.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getGoodSet(Name, Output):-

	List = [],
	getUsedComponents(Name , List, Componentset),
	list_to_set(Componentset, ReversedList),
	reverse(ReversedList, Output).

getUsedComponents(Name, List, Output):-

	findall(Inputs, input(Inputs, _), AnswerList),
	member(Name, AnswerList),!,
	List = Output.
	
getUsedComponents(Name, List, Output2):-

	component(ComponentName, _, [Input1, Input2], Name),
	getUsedComponents(Input1, [ComponentName|List], Output1),
	getUsedComponents(Input2, [ComponentName|Output1], Output2).


% Asserts all possible goodsets. 

/*

GEEFT NOG DUBBELE ANTWOORDEN; ERGENS EEN CUT

*/


assertAllGoodSets:-
	
	findall(EndOutput, measuredOutput(EndOutput, _), EndOutputs),
	assertGoodSets(EndOutputs).

assertGoodSets([]).

assertGoodSets(EndOutputs):-

	member(EndOutput, EndOutputs),
	select(EndOutput, EndOutputs, NewEndOutputs),
	getGoodSet(EndOutput, GoodSet),
	write(GoodSet),
	assert(goodset(EndOutput, GoodSet)),
	assertGoodSets(NewEndOutputs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Hier even een voorbeeldje schrijven van hoe dit gaat.



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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% NB --> makeGrid moet aangeroepen zijn

makeMinimalConflictLists(FalseEndNode, NewestMinimalConflictLists):-
	
	MinimalConflictLists = [],
	component(LastComponentName, _, [Input1,Input2], FalseEndNode),
	append([[LastComponentName]], MinimalConflictLists, NewMinimalConflictLists),
	goFurther(Input1, NewMinimalConflictLists, FalseEndNode, NewerMinimalConflictLists),
	goFurther(Input2, NewerMinimalConflictLists, FalseEndNode, NewestMinimalConflictLists).
	


% Basecase is when inputname is original input.
goFurther(InputName, X,_,X):-
	
	not(component(_,_,_,InputName)),
	((component(_, _, [InputName,_], _));
	(component(_, _, [_, InputName], _))).
	

goFurther(InputName, MinimalConflictLists, FalseEndNode, NewerMinimalConflictLists):-
	
	component(ComponentName, _, [Input1, Input2], InputName),
	forwardChecking(ComponentName, FalseEndNode, MinimalConflictLists, NewMinimalConflictLists).


forwardChecking(ComponentName, FalseEndNode, MinimalConflictLists, NewMinimalConflictLists):-

	findall(PossibleEndNode, (goodset(PossibleEndNode, GoodSet), member(ComponentName, GoodSet)), EndNodes),
	select(FalseEndNode, EndNodes, OtherEndNodes),
	checkCorrectness(OtherEndNodes),
	append([[ComponentName]], MinimalConflictLists, NewMinimalConflictLists),
	write(ComponentName),
	write(' has been added to the minimal-conflict-list.'),nl.


forwardChecking(ComponentName, _, MinimalConflictLists, NewMinimalConflictLists):-!,

	component(ComponentName, _, _, Outputname),
	(component(NextComponent, _, [Outputname, _], _),
	append([[ComponentName, NextComponent]], MinimalConflictLists, AlmostNewMinimalConflictLists),
	component(NextComponent, _, [_, OtherInputFromNextComponent], _),
	component(OtherComponent, _,_,OtherInputFromNextComponent),
	append([[ComponentName, OtherComponent]], AlmostNewMinimalConflictLists, NewMinimalConflictLists));
	
	(component(NextComponent, _, [_,Outputname], _),
	append([[ComponentName, NextComponent]], MinimalConflictLists, AlmostNewMinimalConflictLists),
	component(NextComponent, _, [OtherInputFromNextComponent, _], _),
	component(OtherComponent, _,_,OtherInputFromNextComponent),
	append([[ComponentName, OtherComponent]], AlmostNewMinimalConflictLists, NewMinimalConflictLists)).



checkCorrectness([]).

checkCorrectness([H|EndNodes]):-

	findFaultNodes(FaultNodes),
	member([H,_], FaultNodes),
	checkCorrectness(EndNodes).








/*




selectRandomNodes(InputList, X, Y):-
	member(X, InputList),
	select(X, InputList, NewList),
	member(Y, NewList).

compareExpectedWithMeasured(Output, Input2):-
	measuredInput(Input2, Value),
	Output == Value.

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
*/

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


findCandidates([]).

findCandidates([First|Rest]):-
	askInputFromUser(First),
	findCandidates(Rest).
/*
askInputFromUser([]).

askInputFromUser([FirstElement|Rest]):-
	component(FirstElement,Sort,[Input1,Input2],_),
	not( measuredOutput(Input1, _)),
	not( measuredOutput(Input2, _)),
	write("What is the measuredInput from point: "),
	write(Input1),
	readln(Value1),
	assert(measuredOutput(Input1, Value1 )),
	write("What is the measuredInput from point: "),
	write(Input2),
	readln(Value2),
	assert(measuredOutput(Input2, Value2 )),
	askInputFromUser(Rest).
	
askInputFromUser([_|Rest]):-
	askInputFromUser(Rest).
*/


findProblem([]):-
	write('No faults').
	
findProblem([Head|Tail]):-
	Head = [ComponentName],
	component(ComponentName,_, [Input1,Input2], Output),
	getAllTheValues([Input1, Input2]),
	calculatedNewMeasuredValue(ComponentName, MeasuredValue),
	expectedOutput(Output, ExpectedValue),
	ExpectedValue == MeasuredValue,
	findProblem(Tail).

findProblem([Head|_]):-
	Head = [ComponentName],
	write('the wrong component is '),
	write(ComponentName),
	write(' broke.').

	
calculatedNewMeasuredValue(ComponentName, MeasuredValue):-
	component(ComponentName, ComponentSort, [Input1,Input2],OutputName),
	measuredInput(Input1, Value1),
	measuredInput(Input2, Value2),
	(ComponentSort == adder, MeasuredValue is Value1 + Value2;
	ComponentSort == multi, MeasuredValue is Value1 * Value2),
	assert( measuredInput(OutputName ,MeasuredValue)).

getAllTheValues([]).

getAllTheValues([Input1|Rest]):-
	not( measuredInput(Input1, _) ),
	askInputFromUser(Input1, Value),
	assert( measuredInput(Input1, Value)),
	getAllTheValues(Rest).

getAllTheValues([_|Rest]):-
	getAllTheValues(Rest).

askInputFromUser(Input1, Output):-
	write('What is the measured Input from point: '),
	write(Input1),
	nl,
	read(Output).
	


