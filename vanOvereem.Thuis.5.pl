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

% A circuit consists of components which take two values as input and generate an output with it. This output can be useful as an input for the next component. When having a system, one only knows 1. The original inputvalues, 2. The measured output values  and 3. Whether the components are multipliers or adders. 

% The program can generate the expected in- and output values at every place (node) in the system.

% The generated (expected) outputvalues can differ from the 'real' outputvalues. The computer needs to know the real outputvalues that come out of the circuit, but when testing the circuit for faulty components it can also ask the user for the real (measured) values at certain points in the circuit. The program needs to know this to be able to detect the components that are 'broken' (do not behave as expected).

% The program is dividable in 3 parts: 

%	1. calculating every expected value in the system & check whether system works correctly by matching expected output values with the measured outputvalues.
%	2. making a list of minimal conflicted component sets.
%	3. trying to ask the user smart questions about outputvalues at certain places, in order to detect the wrong components of a broken system.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are the different parts of the system.
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
% These predicates are quite nice to check what the current system looks like.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

% go1 creates the 'standard' system, given in the assignment. It first resets any earlier systems, then asserts 'goodsets'. Every outputvalue has a certain path of components that are responsible for that value to become what it is. These goodsets are used when one needs to check whether a components is part of a certain output-track. This is the case when generating the minimal-conflict-sets later on: one needs to know what the exact influence is of a component in the whole system.

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
	assert( measuredOutput(f, 10)), % <-- wrong value!
	assert( measuredOutput(g, 12)), % <-- right value.
	
	assertAllGoodSets,
	
	makeGrid. % generates all node-values.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the system we made up. 
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
	assert( component(a4, adder, [o, p], q)),

	assertAllGoodSets,
	
	makeGrid.
go3:-

	assert(input(a, 1)),
	assert(input(b, 1)),
	assert(measuredOutput(c, 1)),
	assert(component(a1, adder, [a, b], c)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MakeGrid goes forward to the system adding all the expected
%% output. It goes forward by taking all the input from all the
%% components calculating their expected output. It stops when
%% all the input from all the component has been processed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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


/*

getValueOfOther(ComponentSort, OutputValue, ExpectedInputValue, OtherInputValue):-

	(ComponentSort == multi, backwardMulti(OutputValue, ExpectedInputValue, OtherInputValue);
	ComponentSort == adder, backwardAdder(OutputValue, ExpectedInputValue, OtherInputValue)), !.

backwardMulti(Value, ExpectedValue , Output):-

	Output is Value / ExpectedValue.

backwardAdder(Value, ExpectedValue , Output):-

	Output is Value - ExpectedValue.

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Asserts all 'goodsets' %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assertAllGoodSets:-
	
	findall(EndOutput, measuredOutput(EndOutput, _), EndOutputs),
	assertGoodSets(EndOutputs).

assertGoodSets([]).

assertGoodSets(EndOutputs):-

	member(EndOutput, EndOutputs),
	select(EndOutput, EndOutputs, NewEndOutputs),
	getGoodSet(EndOutput, GoodSet),
	%write(GoodSet),
	assert(goodset(EndOutput, GoodSet)),
	assertGoodSets(NewEndOutputs).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findfaultNodes/1 finds all the wrong endnodes. Wrong endnodes are nodes from which the expectedOutput-value is not the same as their measured value.										
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
%%%%%%%%%%%%%%       makeMinimalConflictLists/2    %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Gives back a conflictlist when you put in a false endnode. It calculated which components could be responsible for the falseness of that endnode. 

/* Query example, after go1. (This is when g has a right output value)

475 ?- go1.
Derived: expectedOutput(x,6)
Derived: expectedOutput(y,6)
Derived: expectedOutput(z,6)
Derived: expectedOutput(f,12)
Derived: expectedOutput(g,12)
true .

476 ?- makeMinimalConflictLists(f, X).
X = [[m2, m3], [m2, a2], [m1], [a1]] .


It doesn't work quite right yet for go2 or maybe other systems with a broader horizon than go1. This is because when it reasons backward from the faulty endnode, it doesn't look back any further than 2 components back.

We had a few ideas about the implementation (using recursion) to make this work, but we didn't have the time to get it quite right.

The important things to consider when wondering whether a component should be put (perhaps with a combination of other components) in the minimal-conflict-set are the following:

- Does the component any how lead to another end node? 
	No -> put it 'alone' in m-c-set.
	Yes -> Are these endnodes false too?
		Yes -> Put 'alone' in m-c-set.
		No -> 
			1. The component should be combined with the next component in the system.
			2. The component should be combined with his 'brother'-component; this is the component that also brings in an output value for the next component.
			3. If all this brother-component has parent-components of itself, then those components can again be responsible for the faultyness of this brother-component. This cna go on and on and on (feel the recursion) again until the original inputs are hit (beginning of system is faced, can't go backward anymore).

*/

% The actual component with the faulty endnode as output should in any case be in the minimal-conflict-set! The component doesn't lead to other components.

makeMinimalConflictLists(FalseEndNode, NewestMinimalConflictLists):-
	
	MinimalConflictLists = [],
	component(LastComponentName, _, [Input1,Input2], FalseEndNode),
	append([[LastComponentName]], MinimalConflictLists, NewMinimalConflictLists),
	goFurther(Input1, NewMinimalConflictLists, FalseEndNode, NewerMinimalConflictLists),
	goFurther(Input2, NewerMinimalConflictLists, FalseEndNode, ReversedLists),
	reverse(ReversedLists, NewestMinimalConflictLists).
	


% Basecase is when inputname is original input.
goFurther(InputName, X,_,X):-
	
	not(component(_,_,_,InputName)),
	((component(_, _, [InputName,_], _));
	(component(_, _, [_, InputName], _))).
	

goFurther(InputName, MinimalConflictLists, FalseEndNode, NewMinimalConflictLists):-
	
	component(ComponentName, _, [_,_], InputName),
	forwardChecking(ComponentName, FalseEndNode, MinimalConflictLists, NewMinimalConflictLists).


forwardChecking(ComponentName, FalseEndNode, MinimalConflictLists, NewMinimalConflictLists):-

	findall(PossibleEndNode, (goodset(PossibleEndNode, GoodSet), member(ComponentName, GoodSet)), EndNodes),
	select(FalseEndNode, EndNodes, OtherEndNodes),
	checkCorrectness(OtherEndNodes), % are the other endnodes false too? This fails when no!
	append([[ComponentName]], MinimalConflictLists, NewMinimalConflictLists).



% The other endnodes are apparently not false; combined components have to be put in the m-c-set.
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

/* Another query example: (Now g is also false: different minimal-conflict-set!

497 ?- retract(measuredOutput(g,12)).
true.

498 ?- assert(measuredOutput(g,10)).
true.

499 ?- makeMinimalConflictLists(f, X).
X = [[a1], [m1], [m2]] .


*/




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% findProblem tries to find wrong components through the   %%
%% the users input. It goes by the minimal-conflict-list and%%
%% ask the users measurements, It add these facts and the   %%
%% calculates the estimated output. It compares this with the%
%% expectedOutput. It does this for every model. 	    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findProblem([]):-

	write('No faults').

findProblem([[ComponentName1, ComponentName2]|Tail]):-

	component(ComponentName1,_, [Input1,Input2], Output1),
	component(ComponentName2,_, [Input3,Input4], Output2),
	getAllTheValues([Input1, Input2]),
	calculatedNewMeasuredValue(ComponentName1, MeasuredValue1),
	expectedOutput(Output1, ExpectedValue1),
	ExpectedValue1 == MeasuredValue1,
	write(ComponentName1),
	write(' is working correctly, and'),
	getAllTheValues([Input3, Input4]),
	calculatedNewMeasuredValue(ComponentName2, MeasuredValue2),
	expectedOutput(Output2, ExpectedValue2),
	ExpectedValue2 == MeasuredValue2,
	write('the component '),
	write(ComponentName2),
	write(' is broken.'),	
	findProblem(Tail).
	
findProblem([[ComponentName1,ComponentName2]|_]):-

	write(ComponentName1),
	write(' and '),
	write(ComponentName2),
	write(' are broken.').


findProblem([[ComponentName]|Tail]):-

	component(ComponentName,_, [Input1,Input2], Output),
	getAllTheValues([Input1, Input2]),
	calculatedNewMeasuredValue(ComponentName, MeasuredValue),
	expectedOutput(Output, ExpectedValue),
	ExpectedValue == MeasuredValue,
	findProblem(Tail).

findProblem([[ComponentName]|_]):-

	write(ComponentName),
	write(' is broken.').

	
calculatedNewMeasuredValue(ComponentName, MeasuredValue):-

	component(ComponentName, ComponentSort, [Input1,Input2],OutputName),
	measuredInput(Input1, Value1),
	measuredInput(Input2, Value2),
	(ComponentSort == adder, MeasuredValue is Value1 + Value2;
	ComponentSort == multi, MeasuredValue is Value1 * Value2),!,
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


% We hope you enjoyed this program.

%	- Casper Thuis & Fije van Overeem























/* BUNCH OF DEAD CODE




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
