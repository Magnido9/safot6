

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
