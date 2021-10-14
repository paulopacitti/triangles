% Receives a list of rectangles in the Cartesian plane and 
% prints the number of intersections between them and their pairs
% Use this to run the program: swipl -q -f triangles.pl -t topo < teste

% data
:- dynamic ret/5.

topo :-
    read(Input), % Reads the input
    record(Input),
    intersection(Input, L),
    list_to_set(L,S), % remove duplicates
    length(S,Length),
    writef('%1L\n', [Length]),
    output(S). % prints as descripted in the project description

% record in the database
record([]).
record([X|R]) :- assertz(X), record(R).

% intersection(-ListOfRectangles, +ListOfRectanglesThaHaveIntersection)
intersection([], _).
intersection([X|R], Pairs) :- 
    findall(ret(Name, X11, X12, Y11, Y12), ret(Name, X11, X12, Y11, Y12), FindResult), % get all rectangles in the database
    include(inrange(X), FindResult, InRangeList), % "filter", +InRangeList contains the names of the rectangles that intercept X
    make_pairs(X, InRangeList, Pairs), % +Pairs is the array of pairs (X, RectangleThatIntercepts)
    intersection(R, Pairs). % recursion

% checks if two rectangles overlap
inrange(ret(_, X11, Y11, X12, Y12), ret(_,X21, Y21, X22, Y22)) :- 
    (X11 =:= X12; Y12 =:= Y11; X21 =:= X22; Y22 =:= Y21; X11 >= X22; X21 >= X12; Y11 >= Y22; Y21 >= Y12)
    -> false
    ; true.

% not working :(, but I'm close.
make_pairs(_, [], _).
make_pairs(ret(Name1, X11, X12, Y11, Y12), [ret(Name2,_,_,_,_)|R], Pairs) :- 
    append(Pairs, [[Name1, Name2]], NewPairs),
    make_pairs(ret(Name1, X11, X12, Y11, Y12), R, NewPairs).

%%%%%%%%%%% Helper Functions %%%%%%%%%%%

% Converts a list to a set, reference code: https://www.swi-prolog.org/pldoc/man?predicate=list_to_set/2
list_to_set(List, Set) :-
    must_be(list, List),
    number_list(List, 1, Numbered),
    sort(1, @=<, Numbered, ONum),
    remove_dup_keys(ONum, NumSet),
    sort(2, @=<, NumSet, ONumSet),
    pairs_keys(ONumSet, Set).

number_list([], _, []).
number_list([H|T0], N, [H-N|T]) :-
    N1 is N+1,
    number_list(T0, N1, T).

remove_dup_keys([], []).
remove_dup_keys([H|T0], [H|T]) :-
    H = V-_,
    remove_same_key(T0, V, T1),
    remove_dup_keys(T1, T).

remove_same_key([V1-_|T0], V, T) :-
    V1 == V,
    !,
    remove_same_key(T0, V, T).
remove_same_key(L, _, L).

output([]).
output([X|R]) :- writef('%1L%w\n', X), output(R).
