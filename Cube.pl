main :-
    generator4(T), tester4(T), write(T),nl.
%generator takes too long but does work when the input range is limited, i.e. 9000,9800
%tester works perfectly and allows x_tester4(N) to pass all tests and return 10

x_tester4( N ) :-
  x_tester4_loop(
  [ [[8 ,2 ,7] ,[6 ,1] ,[5 ,3] ,[4 ,0 ,9]]
  , [[8 ,2 ,7] ,[6 ,1] ,[4 ,0 ,9] ,[5 ,3]]
  , [[8 ,2 ,7] ,[5 ,3] ,[6 ,1] ,[4 ,0 ,9]]
  , [[8 ,2 ,7] ,[4 ,0 ,9] ,[6 ,1] ,[5 ,3]]
  , [[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]
  , [[6 ,1] ,[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7]]
  , [[5 ,3] ,[6 ,1] ,[4 ,0 ,9] ,[8 ,2 ,7]]
  , [[5 ,3] ,[4 ,0 ,9] ,[6 ,1] ,[8 ,2 ,7]]
  , [[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7] ,[6 ,1]]
  , [[4 ,0 ,9] ,[8 ,2 ,7] ,[6 ,1] ,[5 ,3]] ] , 0 , N ).

x_tester4_loop( [] , C , C ).
x_tester4_loop( [ T | TS ] , C , N ) :-
  tester4( T ),
  C1 is C + 1,
  x_tester4_loop( TS , C1 , N ).
x_tester4_loop( [ _ | TS ] , C , N ) :-
x_tester4_loop( TS , C , N ).

x_generator4( N ) :-
  x_generator4_loop(
  [ [[9,6,7],[4,0,1],[2,8,3],[5]]
  , [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
  , [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
  , [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
  , [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
  , [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
  , [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]] 
  , [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]] 
  , [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]] 
  , [[3] ,[5] ,[6 ,0 ,7] ,[2] ,[4 ,1] ,[8 ,9]] ] , 0 , N ).

x_generator4_loop( [] , C , C ).
x_generator4_loop( [ T | TS ] , C , N ) :-
	generator4( T ),
	C1 is C + 1,
	x_generator4_loop( TS , C1 , N ).
x_generator4_loop( [ _ | TS ] , C , N ) :-
	x_generator4_loop( TS , C , N ).
    

generator4(T) :-
    % if the last digit is even and not equal to 2, then the sequence can't be shown as primes as
    %	 every even number is non-prime except for 2
    % if a given number cannot produce a sequence, it means its first four digits cannot be turned into a prime
    %	whether you do first digit, first two digits, etc, so you need to skip by several million to the
    %	next four digits
    % using 8954 --> 9851 will find 9 of the 10 required sequences for x_generator
    % using 3560 --> 3561 finds the remaining sequence for the x_generator
    %constructing potential 'stems' - first four digits
    %smallest possible valid stem = 1032, largest = 9871
    %between(3560, 3561, X),
    between(1032,9871, X),	
    digits(X, Y),
    stemPrime(Y),	%check the stem's digits are unique to eachother and that primes can be found
    % stem is established, now we need to permute the missing digits into the list and check the sequence
    missingDigits(Y, Z),
    perm(Z, A),	%permutes the remaining digits
    append(Y, A, B), %creates the full ten digit sequence to check for primes -> will contain all ten unique digs
    last_dig(B, C),	%we use this to check that it is either odd or ends in a two
 	%					--> is the last digit a member of the set [1,2,3,5,7,9] ???
    member(C, [1,2,3,5,7,9]),
    %this checks it can be composed as primes
    seq(B, 1, T). 




	  
%this version passes 8 of the tests instead of ten but will successfully generate the solution unlike the above which passes 9 tests
seq([HS|TS], D, [X|XS]) :-
      % this function is used to check that a ten-digit sequence can be shown as a collection of primes
      % D specifies how much of the sequence to check for primeness, i.e. first two digits as a number, first digit, etc.
      split_list(D, [HS|TS], Y, YS),
      digList(Y, Z),
      %digList will fail if there are leading zeroes in a component as something like 2 will be prime whereas 02
      %    should explicitly not be considered prime
      (	prime(Z) ->  
          % front of list is prime so we now check following digits
          digits(Z, X),
          length_list(YS, V),
          (   V =\= 0 ->  
          	  %find valid subsequences
          	  seq(YS,1,XS);
              seq(YS,2,XS);
              seq(YS,3,XS);
          	  seq(YS,4,XS);
          !
          );
      !,
      	  (   D =< 3 ->  
              seq([HS|TS], D+1, [X|XS]);
              fail
          )
      ).

stemPrime([X1,X2,X3,X4]) :-
    % used to check if the 'stem' (first four digits) can be written as primes without repeating digits
    X1 \= X2, X1 \= X3, X1 \= X4, X2 \= X3, X2 \= X4, X3 \= X4,
    (  prime(X1) ->  
    	true, !;
    	digList([X1,X2], X),
        (   prime(X) ->  
        	true, !;
        	digList([X1,X2,X3], Y),
            (   prime(Y) ->  
            	true, !;
            	digList([X1,X2,X3,X4], Z),
                (   prime(Z) ->  
                	true, !;
                	fail
            	)
        	)
    	)
   	).

%difference of two sets - used for finding missing digits to permute into the full ten-digit sequence
difference([], _, []).
difference([X|XS],YS,W):-
    member(X,YS),
    difference(XS,YS,W).
difference([X|XS], YS, [X|W]):-
    \+ member(X, YS),
    difference(XS,YS,W).

%need to check which digits are not used in the stem so we can permute them
missingDigits(X, Y) :-
    numlist(0,9,N),
    difference(N, X,Y).

% the tester needs to:
% - turn the list of digits of primes into a list of numbers --> [6,1] = 61
% - order the list of numbers largest to smallest (descending order) --> need a sorting algorithm
% - discard the smallest number
% - see if cubes can be formed --> copy the form of seq used for primes but change it to check if numbers are cubes
% EXAMPLE ::: [[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]
% ====> [61, 827, 409, 53] ===> [827,409,61] ===> [[8], [2,7], [4,0,9,6], [1]] ===> test passed
% tester4([[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]).
tester4(X) :-
    digitsAsNums(X, Y),
    bubble_sort(Y, Z),
    %now the list is sorted, we need to remove the last prime
    length_list(Z, L),
    split_list(L-1, Z, A, _),	%we only need to work with the front of the list, A
    numsToDigs(A, B), !,
    cubesGenerateable(B).

%cubes a list of elements; used to create the cubes set for checking membership of a number and verifying
%		if it is a cube or not
cube([], []).
cube([X|XS], [Y|YS]) :-
    Y is X * X * X,
    cube(XS,YS).

%checks a number is a cube by checking it is a member of the set of cubes from 1^3 to 21^3 
%		as 21^3 is the largest four-digit perfect cube
cubeChecker(X) :-
    numlist(1,21,A),
	cube(A, B),
	member(X, B).

% returns true if a sequence of cubes can be formed and false if otherwise
cubesGenerateable(X) :-
    cubes(X, 1, _).

cubes([X], _, [X]) :- 
    cubeChecker(X).
cubes([HS|TS], D, [X|XS]) :-
%D specifies how much of the sequence to check for cubes, i.e. first two digits as a number, first digit, etc.
      split_list(D, [HS|TS], Y, YS),	
      digList(Y, Z),
      (	cubeChecker(Z) -> 
          % front of list is a cube so we now check following digits
          digits(Z, X),
          length_list(YS, V),
          (   V =\= 0 ->  
              cubes(YS, 1, XS);
              %no more digits left to check
              %XS is Y
              !
          );
          (   D =< 3 ->  
              cubes([HS|TS], D+1, [X|XS]), !; %  ! tells the program to stop backtracking after a solution is found
              fail
          )
      ).
    

numsToDigs([], []).
numsToDigs([X], Y) :-
    digits(X, Y).
numsToDigs([X|XS], [Y|YS]) :-
    digits(X, Z),
    numsToDigs(XS, B),
    append(Z, B, [Y|YS]).
    
pass([], []).
pass([X], [X]).
pass([X0,X1|XS], [X0|Rem]) :-
    X0 >= X1, !,	
    pass([X1|XS], Rem).
pass([X0,X1|XS], [X1|Rem]) :-
    pass([X0|XS], Rem).

bubble_sort(L, SL) :-
    pass(L, L0),	%performs a pass on the list to swap consecutive elements if not in descending order
    (ordered(L0) ->
        SL = L0;	% sorted list SL is equal to the current list if it is ordered
    	bubble_sort(L0, SL)	% if not, then another bubblesort is performed
    ).

% ordered is a predicate to check if a list is in descending order
ordered([]).
ordered([_]) :- !.
ordered([X0,X1|R]) :-
    X0 >= X1,	%checks each element with its adjacent element to see that it is in descending order
    ordered([X1|R]).
    	

% This is for converting a 2D list consisting of lists of digits into a 1D list of numbers
digitsAsNums([], []).
digitsAsNums([[X]], [[Y]]) :-
    digList(X, Y).
digitsAsNums([X|XS], [Y|YS]) :-
    digList(X, Y),
    digitsAsNums(XS, YS).
    
perm([],[]).
perm(L,[H|T]) :-
 append(V,[H|U],L),
 append(V,U,W), perm(W,T).

last_dig(X, Y) :-
    length_list(X, Z),
    split_list(Z-1, X, _, B),
    digList(B, Y).

digits(N, [N]) :-
    N < 10.
digits(N, W) :-
    N >= 10,
    divmod(N, 10, D, M),
    digits(D, R),
    append(R, [M], W).

divmod(A, B, D, M) :-
    M is A mod B,
    D is A div B.
        

factorable(X):-
    Y is X div 2,
	between(2, Y, Z),
    0 is X mod Z.

length_list([], 0).
length_list([_|T], N) :-
    length_list( T, W ),
    N is W + 1.

prime(X) :-
    digits(X, [H|_]),
    H > 0,
    X > 1,
    \+ factorable(X).

digList([X], X).
digList([A,B], X) :-
    A \= 0,
    X is A * 10 + B.
digList([A,B,C], X) :-
    A \= 0,
    X is A * 100 + B * 10 + C.
digList([A,B,C,D],X) :-
    A \= 0,
    X is A * 1000 + B * 100 + C * 10 + D.

split_list( _ , [] , [] , [] ).
split_list( 0 , X , [] , X ).
split_list( N, [ H | T ], [ H | AS ], BS) :-
	N1 is N-1,
	split_list( N1, T, AS, BS).


