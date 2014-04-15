%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- consult(beta_convert).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% question 1 %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% DCG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s(SemNP*SemVP) --> np(SemNP), vp(SemVP).

np --> d(X), nom(X).

nom(X) --> n(X).
nom(X) --> a, nom(X).

vp --> vt, np.
vp --> vi.

d(zij) --> [iedere];[de].
d(onz) --> [het].
% d --> [iedere].
n(onz) --> [kind].
n(zij)--> [man];[vrouw].
a --> [mooie];[aardige];[grote];[blonde].
vt --> [ziet].
vi --> [slaapt].
vi(slaapt, X^slaap(X) ).

%%%%%%%%%%%%%%%%% FILES FROM LAMBDA_DCG.pl %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interpret(Sentence, Sem) :- s(Sem, Sentence, []).

logic(Sentence, Logic) :- 
    interpret(Sentence, Sem),
    convert(Sem, Logic).
