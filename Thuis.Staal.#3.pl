:- dynamic fact/1.
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

% Superclass stands in front, if we need all the properties from a refined
% disease you remove the first propertie and we are left whit all the
% properties of a disease  
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

%List all superdiseases to seprate it whit refined disease.
sd(malaria).
sd(darminfectie).
sd(huidziekte).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main function it doesn't do the data refinement   %
% since we let the user input the symptoms that are     %
% are needed to match it whit a superclass in the       %
% disease tree.'                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go:-
    write( 'Geef een aantal van de symptomen die hier beneden staan. Scheid de symptomen met een spatie en eindig met een punt.' ),
    nl,
    symptomsSuperclasses(List),
    writeSymptoms(List),
    getsentence(Input),
    write(Input),
    addFacts(Input),
    questions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find all the properties of the superclasses and returns%
% a list of them.                                        %                                                       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


symptomsSuperclasses(List):-
    findall(Superclasses, sd(Superclasses), ListSup),
    findPropSuperclasses(ListSup, List3),
    toAtoms(List3, List2),
    list_to_set(List2, List).

findPropSuperclasses([], []).

findPropSuperclasses([H|Rest], [X|List]):-
    if X then H,
    findPropSuperclasses(Rest, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writes down symptoms to interface.                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeSymptoms([]).

writeSymptoms([FirstSymptom|List]):-
    write('Heb je dit symptoom : '),
    write(FirstSymptom),
    nl,
    writeSymptoms(List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% toAtoms removes the list of all the and and or operaters%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toAtoms([], []).

toAtoms([H|Rest], [H|List]):-
    atom(H),
    toAtoms(Rest, List).

toAtoms([H|Rest], List):-
    (H = X and Y;
    H = X or Y), 
    toAtoms([X,Y|Rest], List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Questions predicate returns all the questions that    %
% are relavant given the active superclasses. The       %
% superclasses  are active when a user gives the        %
% symptoms that relate whit that superclass. After that %
% the questions are send to askQuestions(to transfer)   %
% the properties of a disease into a question until     % 
% either a disease is found or the questions run out    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
 
questions:-
    findall(Superclasses, fact(Superclasses), ListFacts),
    checkSuperclasses(ListFacts, ListSups),
    checkSups(ListSups).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Checks whether the fact is a superclass if it is it add%
% the Fact                                              % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkSuperclasses([],[]).

checkSuperclasses([H|Rest], [H|List]):-
    sd(H),
    checkSuperclasses(Rest, List).

checkSuperclasses([_|Rest], List):-
    checkSuperclasses(Rest, List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Checks whether the fact is a superclass if it is it add%
% the Fact                                              % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkSups([]):- fail.

checkSups([H|_]):-
    askQuestions(H).

checkSups([_|Rest]):-
    checkSups(Rest).
 
askQuestions(Superclass):-
    getDecomposedLists(Superclass, List),
    findDisease(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This predicate is the refinement process, it       %
%% continues till either the list is empty or the     %
%% disease is found                                   % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findDisease([]):- fail.

findDisease([H|_]):-
    writeSymptoms(H),    
    getsentence(Input),
    write(Input),
    addFacts(Input),
    checkForDisease.

findDisease([_|List]):-
    findDisease(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkForDisease together whit searchDisease checks   %
%% whit the whether a newly added fact is disease. It   %
%% does this by check whether the fact is End Conclusion%
%% and it check is it not in the premises. It also      %
%% if its not a superclass. If all checks out it writes % 
%% the disease                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% getDecomposedLists you give a Superclass and         %%
%%returns a List whit all the premises where superclass %%
%% is is part of the premise, in other words it gives all%
%% the properties of all the diseases that fall under   %%
%% the superclass                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getDecomposedLists(Superclass, DecList):-
    findall(Premises, if Premises then _, List),
    decomposeList(Superclass, List, DecList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decomposeList gets a list a decomopeses all the      %
%% and , or operators out of it and returns a list again%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decomposeList(_, [], []).

decomposeList(Superclass, [H|List], [Rest|DecList]):-
    toAtoms([H], [X|Rest]),
    Superclass == X,
    decomposeList(Superclass, List, DecList).

decomposeList(Superclass, [_|List], DecList):-
    decomposeList(Superclass, List, DecList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% addFacts get a list of symptoms and add them as      %%
%% as fact when the list is empty                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addFacts([]):- forward.

addFacts([H|List]):- 
    assert(fact(H)), 
    addFacts(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getsentence is a predicate from bratko we use to  %%
%% get the user input from and return it into a list %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ATTENTION ATTENTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ons huiswerk was bijna klaar totdat wij op dit     %
% probleem stuiten, waar prolog ons te veel in de weg%
% zat om dat het niet interface vriendelijk is.      %
% Het probleem staat hier beneden weer geven. Blijkbaar
% doet getsentence het goed echter zet bij het vragen%
% naar symptoms komt er een spatie of een "new line" %
% teken voor de symptom waar door prolog het niet    %
% meer kan matchen.  Wij wisten niet hoe we dit      %
% moesten oplossen, toch willen laten zien dat het   %
% meer aan prolog ligt dat ons belemmerd             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Geef een aantal van de symptomen die hier beneden staan. Scheid de symptomen met een spatie en eindig met een punt.
Heb je dit symptoom : koorts
Heb je dit symptoom : gebeten
Heb je dit symptoom : diarree
Heb je dit symptoom : jeuk
|: koorts.
[koorts]Derived:malaria
Derived:darminfectie
No more facts
Heb je dit symptoom : hogekoorts
Heb je dit symptoom : hogepieken
|: hogekoorts hogepieken.
[
hogekoorts,hogepieken]No more facts
Heb je dit symptoom : hogekoorts
Heb je dit symptoom : dagenkoorts
|: 
?- fact(hogekoorts).
false.

?- fact(hogepieken).
true.
