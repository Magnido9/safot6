:- use_module(library(clpfd)).

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
