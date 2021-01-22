:- use_module(library(clpfd)).

%Q1


card(_,_).
card_suit(spades,3).
card_suit(clubs,1).
card_suit(diamonds,4).
card_suit(hearts,2).


lowest([X],X) :- !.
lowest([card(Number,Suit),card(Number2,Suit2)|Tail], X):-
    ( Number < Number2 ->
        lowest([card(Number,Suit)|Tail],X)
    ;
    	Number#=Number2->
    	card_suit(Suit,A1),
        card_suit(Suit2,A2),
        	(   A1<A2->
        	lowest([card(Number,Suit)|Tail],X)
    	;
        	lowest([card(Number2,Suit2)|Tail],X)
            )
    ;
        lowest([card(Number2,Suit2)|Tail],X)
    ).
filter(_,[],X,X) :- !.
filter(card(Number2,Suit2),[card(Number,Suit)|Tail], X,N):-
    ( Number > Number2 ->
        filter(card(Number2,Suit2),Tail,[card(Number,Suit)|X],N)
    ;
    	Number#=Number2->
    	card_suit(Suit,A1),
        card_suit(Suit2,A2),
        	(   A1<A2->
        	filter(card(Number2,Suit2),Tail,X,N)
    	;
        	filter(card(Number2,Suit2),Tail,[card(Number,Suit)|X],N)
            )
    ;
        filter(card(Number2,Suit2),Tail,X,N)
    ).

filter(A,B,C):-
    filter(A,B,[],C).


cmp_card(card(Number,Suit),card(Number2,Suit2),N):-
	( Number < Number2 ->
         N is 1
    ;
    	Number#=Number2->
    	card_suit(Suit,A1),
        card_suit(Suit2,A2),
        	(   A1<A2->
        	N is 1
    	;
            A1=A2->
        	N is 0
        ;
        	N is 2
            )
    ;
        N is 2
    )

.
winner(H1,H2, C,X):-
    filter(C,H1,C1),
    filter(C,H2,C2),
    lowest(C1,X1),
    lowest(C2,X2),
    cmp_card(X1,X2,X)
.

%Q2:
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
  
%Q3:
change(0,_,X,Y) :- X=Y, !.
change(X,100,Z,N):-change(X,50,Z,N).
change(X,100,Z,N):-X#>=100->  X1 is X-100,append(Z,[100],A),change(X1,100,A,N).
change(X,50,Z,N):-change(X,10,Z,N).
change(X,50,Z,N):-X#>=50->  X1 is X-50,append(Z,[50],A),change(X1,50,A,N).
change(X,10,Z,N):-change(X,5,Z,N).
change(X,10,Z,N):-X#>=10->  X1 is X-10,append(Z,[10],A),change(X1,10,A,N).
change(X,5,Z,N):-change(X,1,Z,N).
change(X,5,Z,N):-X#>=5->  X1 is X-5,append(Z,[5],A),change(X1,5,A,N).
change(X,1,Z,N):-X#>=1->  X1 is X-1,append(Z,[1],A),change(X1,1,A,N).

change(X,Y):-
    change(X,100,[],Y).
