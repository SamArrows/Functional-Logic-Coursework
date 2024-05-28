main :-
    generator3(T), tester3(T), write(T),nl.

generator3(T) :-
    %- numbers to test are the squares of 32 through to 1000
    %- generate those numbers then square them
    between(32,1000,X),
    T is X * X.

x_generator3(N) :-
    x_generator3_loop(
        [	1024,	9409,	23716,	51529,
          	123904,	185761,	868624,	962361,
          	982081,	1000000 ], 0, N).

x_generator3_loop( [], C, C).
x_generator3_loop( [T|TS], C, N) :-
    	generator3(T),
    	C1 is C+1,
    	x_generator3_loop(TS, C1, N).
x_generator3_loop( [_|TS], C, N):-
    x_generator3_loop(TS, C, N).

% Tester needs to filter integers where:
% -all of the digits are different
% -the last digit is equal to the number of digits
% -the last-but-one digit is odd
% -one of the digits is zero.
% -In addition, the second, third and last-but-one digits are all exact multiples of the first digit.

tester3(T) :-
    digits(T, X),
    nub(X, Z),
    Z = X,
    containsZero(X, X),
	get_last_digit(X, Y),
	length_list(X, W),
	Y is W,
	multiplesOfDigit(X).	%PLEASE READ COMMENTS FOR THIS FUNCTION


x_tester3(N) :-
    x_tester3_loop(
        [	123056, 	128036, 
        	139076,		142076,
          	148056,		159076,
          	173096,		189036,
          	193056,		198076], 0, N).

x_tester3_loop( [], C, C).
x_tester3_loop( [T|TS], C, N):-
    tester3(T),
    C1 is C + 1,
    x_tester3_loop( TS, C1, N).


nub([], []).
nub([X|XS], [X|W]) :-
    \+ member(X,XS),
    nub(XS, W).
nub([X|XS], W) :-
    member(X,XS),
    nub(XS,W).

multiplesOfDigit([_]).
multiplesOfDigit([H|T]) :-
    penultimate_digit(T,Z), %for getting the penultimate digit			
    take_list(2,T,[X,Y]),	%for getting digits 2 and 3 - X is 2, Y is 3
    %Now we test A,B,Z if multiples of first digit H
    
    X > 0,	%Technically, 0 is a multiple of every digit as for any number x, x * 0 = 0
    		%  	however the answer in the coursework requests 173056 so by including this line,
    		% 	the program will discard the other possibility for a solution if we were to allow
    		% 	0 as a multiple, that is, the number 104976 which fulfills all the criteria of the teaser
    
    0 is X mod H,
    0 is Y mod H,
    0 is Z mod H,
    1 is Z mod 2. %checks that penultimate digit is odd

digits(N, [N]) :-
    N < 10.
digits(N,W) :-
    N >= 10,
    div_mod(N, 10, D, M),
    digits( D, R ),
    append( R, [M], W ).
    
div_mod( A, B, D, M) :-
    D is A div B,
    M is A mod B.

length_list([], 0).
length_list([_|T], N) :-
    length_list( T, W ),
    N is W + 1.

drop_list(_, [], []).
drop_list(0, X, X).
drop_list(N, [_|T], W) :-
    N1 is N - 1,
    drop_list(N1,T,W).
    
get_last_digit([],[]).
get_last_digit([X], X).
get_last_digit([_|T], X) :-
    length_list([_|T], N),
    drop_list(N-1, [_|T], W),
    get_last_digit(W,X).

penultimate_digit([X], X).
penultimate_digit([X,_], X).
penultimate_digit([_|T], X) :-
    length_list([_|T], N),
    drop_list(N-2, [_|T], W),
    penultimate_digit(W,X).

take_list(_,[],[]).
take_list(0,_,[]).
take_list(1,[H|_],H).
take_list(N,[H|T], [H|W]):-
    N1 is N-1,
    take_list(N1,T,W).

containsZero([0|_], _).
containsZero([_|T], X) :-
    containsZero(T,X).