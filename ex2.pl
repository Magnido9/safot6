:-use_module(library(clpfd)).
replace([_|XS],0,Y,[Y|XS]).
replace([X|XS],I,Y,XW):-
    NI is I-1,
    replace(XS,NI,Y,W),
    append([X],W,XW).



action(_,_,_,_,l).
action(_,_,_,_,r).
action(_,_,_,_,h).

tape([_|_],_).

index(tape([L|_],0),L).
index(tape([_|LS],I),X):- 
    index(tape(LS,J),X),
	J #= I-1.

len([], 0).
len([_|Tail], N) :-
	len(Tail, N1),
	N is 1 + N1.

tzicli([],[]).
tzicli([A|B],X):-
       append(B,[A],X).




turingAux(_,CS,[action(CS,_,READ,WRITE,h)|_],tape(LIST,INDEX),tape(NEW_L,INDEX)):-
    index(tape(LIST,INDEX),READ),
    replace(LIST,INDEX,WRITE,NEW_L),!.

turingAux(_,CS,[action(CS,NS,READ,WRITE,r)|NA],tape(LIST,INDEX),X):-
    index(tape(LIST,INDEX),READ),
    replace(LIST,INDEX,WRITE,NEW_L),
    J#=INDEX+1,
    turingAux(0,NS,[action(CS,NS,READ,WRITE,r)|NA],tape(NEW_L,J),X),!.

turingAux(_,CS,[action(CS,NS,READ,WRITE,l)|NA],tape(LIST,INDEX),X):-
    index(tape(LIST,INDEX),READ),
    replace(LIST,INDEX,WRITE,NEW_L),
    J#=INDEX-1,
    J#>=0,
    turingAux(0,NS,[action(CS,NS,READ,WRITE,l)|NA],tape(NEW_L,J),X),!.

    
turingAux(T,CS,[action(S,NS,READ,WRITE,D)|NA],tape(LIST,INDEX),X):-
	tzicli([action(S,NS,READ,WRITE,D)|NA],ACL),
    NT#=T+1,
    len([action(S,NS,READ,WRITE,D)|NA],ACTION_NUM),
    NT#<ACTION_NUM+1,
    turingAux(NT,CS,ACL,tape(LIST,INDEX),X).


turing(S,AC,TAPE,X):-
    turingAux(0,S,AC,TAPE,X).
