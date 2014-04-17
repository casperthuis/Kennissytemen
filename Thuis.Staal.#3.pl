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
    write( ' koorts, hoofdpijn, diarree, bloed-ontlasting, cysten-ontlasting, jeuk,  rode-vlekken  : ' ),  
    getsentence(Input),
    write(Input),
    addFacts(Input).
    
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
if hogekoorts then malariatertiana.    
    
 

addFacts([]):- forward. 

addFacts([H|List]):- 
    assert(fact(H)), 
    addFacts(List).