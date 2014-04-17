
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
    fact( Condition ).da

composed_fact( Condition1 and Condition2 ):-
    composed_fact( Condition1 ),
    composed_fact( Condition2 ).

composed_fact( Condition1 or Condition2 ):-
    composed_fact( Condition1 )
    ;
    composed_fact( Condition2 ).


go:-
    write( 'Geef 1 van de syntomen die hier beneden staan. Scheid de syntomen met een spatie en eidnig met een punt.' ),
    nl,
    symptomsSuperclasses(List),
    write(List),
    getsentence(Input),
    write(Input),
    addFacts(Input).

symptomsSuperclasses([X,Y,Z]):-
    if X then malaria,
    if Y then darminfectie,
    if Z then huidziekte.

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


if koorts then malaria.
if diarree then darminfectie.
if jeuk then huidziekte.

if malaria then malaria-tertiana or malaria-tropica.

if hoge-koorts and hoge-pieken then malaria-tertiana.
if hoge-koorts and 3-dagen-koorts then malaria-tropica.
if hoge-koorts and diarree-perdag and hevige-krampen then bacillaire-dysenterie.
if bloedslijm and diarree and cysten then amoeben-dysenterie. %vraag over diarree 
if hoge-koorts and diarree then tyfus.
if rode-jeukende-plekken and licht-schilferende-huid and jeuk then schimmels.
if jeukende-huiduitslag and jeuk then mijten.
if jeukende-rode-pukkels and jeuk then prickly-heat.    
   
    

addFacts([]):- forward. 

addFacts([H|List]):- 
    assert(fact(H)), 
    addFacts(List).
