% Receives a list of rectangles in the Cartesian plane and 
% prints the number of intersections between them and their pairs
% Use this to run the program: swipl -q -f triangles.pl -t topo < teste

% data
:- dynamic ret/5.

topo :-
    read(Input), % Reads the input
    record(Input),
    intersect(Input, L),
    list_to_set(L,S), % remove duplicates
    length(S,Length),
    writef('%1L\n', [Length]),
    output(S). % prints as descripted in the project description

% record in the database
record([]).
record([X|R]) :- assertz(X), record(R).

% intersection(-ListOfRectangles, +ListOfRectanglesThaHaveIntersection)
intersect([], []).
intersect([X|R], Pairs) :- 
    findall(ret(Name, X11, X12, Y11, Y12), ret(Name, X11, X12, Y11, Y12), FindResult), % get all rectangles in the database
    include(inrange(X), FindResult, InRangeList), % "filter", +InRangeList contains the names of the rectangles that intercept X
    make_pairs(X, InRangeList, NewPairs), % +Pairs is the array of pairs (X, RectangleThatIntercepts)
    append(NewPairs, ListOfPairs, Pairs),
    intersect(R, ListOfPairs). % recursion

% checks if two rectangles overlap
inrange(ret(_, X11, Y11, X12, Y12), ret(_,X21, Y21, X22, Y22)) :- 
    (X11 =:= X12; Y12 =:= Y11; X21 =:= X22; Y22 =:= Y21; X11 >= X22; X21 >= X12; Y11 >= Y22; Y21 >= Y12)
    -> false
    ; true.

make_pairs(_, [], []).
make_pairs(ret(Name1, _, _, _, _), [ret(Name2,_,_,_,_)|R], Pairs) :-
    append([[Name1, Name2]], NewPairs, Pairs),
    make_pairs(ret(Name1, _, _, _, _), R, NewPairs), !.

%%%%%%%%%%% Helper Functions %%%%%%%%%%%
list_to_set([],[]).
list_to_set([[X,Y]|R],Set) :- 
    (X=Y;memberchk([X,Y],R);memberchk([Y,X],R))
    -> list_to_set(R, Set)
    ; append([[X,Y]], NewSet, Set),
      list_to_set(R, NewSet).
    
output([]).
output([X|R]) :- writef('%1L%w\n', X), output(R).
