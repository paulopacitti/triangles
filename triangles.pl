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
intersection([X|R], _) :- 
    findall(ret(Name, X11, X12, Y11, Y12), ret(Name, X11, X12, Y11, Y12), FindResult), % get all rectangles in the database
    include(inrange(X), FindResult, InRangeList), % "filter", +InRangeList contains the names of the rectangles that intercept X
    make_pairs(X, InRangeList, Pairs), % +Pairs is the array of pairs (X, RectangleThatIntercepts)
    intersection(R, Pairs). % recursion

% draft functor, not working
inrange(ret(_, X11, X12, Y11, Y12), ret(_,X21, X22, Y21, Y22)) :- true.

% not working :(, but I'm close.
make_pairs(_, [], _).
make_pairs(ret(Name1,X11, X12, Y11, Y12), [ret(Name2,_,_,_,_)|R], Pairs) :- 
    append(Pairs, [[Name1, Name2]], NewPairs),
    write(NewPairs).
    make_pairs(ret(Name1,X11, X12, Y11, Y12), R, NewPairs).

output([]).
output([X|R]) :- writef('%1L%w\n', X), output(R).