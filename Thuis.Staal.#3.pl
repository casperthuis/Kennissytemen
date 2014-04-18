:- dynamic fact/1.

fact(a).

/* --- Defining operators --- */

:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).


/* --- A simple backward chaining rule interpreter --- */

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


/* --- A simple forward chaining rule interpreter --- */

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


go:-
    write( 'Geef een aantal van de symptomen die hier beneden staan. Scheid de symptomen met een spatie en eindig met een punt.' ),
    nl,
    symptomsSuperclasses(List),
    writeSymptoms(List),
    getsentence(Input),
    write(Input),
    addFacts(Input),
    questions.

writeSymptoms([]).

writeSymptoms([FirstSymptom|List]):-
    write('Heb je dit symptoom : '),
    write(FirstSymptom),
    nl,
    writeSymptoms(List).

sd(malaria).
sd(darminfectie).
sd(huidziekte).

symptomsSuperclasses(List):-
    findall(Superclasses, sd(Superclasses), ListSup),
    findPropSuperclasses(ListSup, List3),
    toAtoms(List3, List2),
    list_to_set(List2, List).

findPropSuperclasses([], []).

findPropSuperclasses([H|Rest], [X|List]):-
    if X then H,
    findPropSuperclasses(Rest, List).

toAtoms([], []).

toAtoms([H|Rest], [H|List]):-
    atom(H),
    toAtoms(Rest, List).

toAtoms([H|Rest], List):-
    (H = X and Y;
    H = X or Y), 
    toAtoms([X,Y|Rest], List).


%Superclass staat vooraan
if koorts or gebeten then malaria.
if diarree or koorts then darminfectie.
if jeuk then huidziekte.
if malaria and hogekoorts and hogepieken then malariatertiana.
if malaria and hogekoorts and dagenkoorts then malariatropica.
if darminfectie and hogekoorts and diarreeperdag and hevigekrampen then bacillairedysenterie.
if darminfectie and bloedslijm and diarree and cysten then amoebendysenterie. %vraag over diarree 
if darminfectie and hogekoorts and diarree then tyfus.
if huidziekte and rodejeukendeplekken and lichtschilferendehuid and jeuk then schimmels.
if huidziekte and jeukendehuiduitslag and jeuk then mijten.
if huidziekte and jeukenderodepukkels and jeuk then pricklyheat.    
   
% Search all facts and 
questions:-
    findall(Superclasses, fact(Superclasses), ListFacts),
    checkSuperclasses(ListFacts, ListSups),
    checkSups(ListSups).

checkSuperclasses([],[]).

checkSuperclasses([H|Rest], [H|List]):-
    sd(H),
    checkSuperclasses(Rest, List).

checkSuperclasses([_|Rest], List):-
    checkSuperclasses(Rest, List).

checkSups([]):- fail.

checkSups([H|_]):-
    askQuestions(H).

checkSups([_|Rest]):-
    checkSups(Rest).

askQuestions(Superclass):-
    getDecomposedLists(Superclass, List),
    findDisease(List).

%Vindt een ziekte als het een fact is en deze fact niet als premisse 
%bekend is maar wel als een goal.

findDisease([]):- fail.

findDisease([H|_]):-

    %Slimme vragen stellen.

    writeSymptoms(H),    
    getsentence(Input),
    write(Input),
    addFacts(Input),
    checkForDisease.

findDisease([_|List]):-
    findDisease(List).

checkForDisease:-
    findall(Facts, fact(Facts), ListOfFacts),
    searchDisease(ListOfFacts).

searchDisease([H|_]):-
    if _ then H,
    findall(Superclass, sd(Superclass), [X,Y,Z]),
    findall(DecList, getDecomposedLists(X, DecList), A),
    findall(DecList, getDecomposedLists(Y, DecList), B),
    findall(DecList, getDecomposedLists(Z, DecList), C),
    append(A,B,D),
    append(D,C, AppendableList),
    flatten(AppendableList, PremiseList),
    \+ member(H, PremiseList),
    \+ sd(H),
    write( 'Je hebt de ziekte : '),
    write( H ),
    nl.

searchDisease([_|Rest]):-
    searchDisease(Rest).

getDecomposedLists(Superclass, DecList):-
    findall(Premises, if Premises then _, List),
    decomposeList(Superclass, List, DecList).

decomposeList(_, [], []).

decomposeList(Superclass, [H|List], [Rest|DecList]):-
    toAtoms([H], [X|Rest]),
    Superclass == X,
    decomposeList(Superclass, List, DecList).

decomposeList(Superclass, [_|List], DecList):-
    decomposeList(Superclass, List, DecList).

addFacts([]):- forward.

addFacts([H|List]):- 
    assert(fact(H)), 
    addFacts(List).


getsentence(Input) :- 
    get0(Char), 
    getrest(Char,Input). 

getrest(46,[]) :-!. 

getrest(32,Input) :-!, 
    getsentence(Input). 

getrest(Letter,[Word|Input]) :- 
    getletters(Letter,Letters,Nextchar), 
    name(Word,Letters), 

getrest(Nextchar,Input). 

getletters(46,[],46):-!. 
getletters(32,[],32):-!. 

getletters(Let,[Let|Letters],Nextchar) :- 
    get0(Char), 
    getletters(Char,Letters,Nextchar).

