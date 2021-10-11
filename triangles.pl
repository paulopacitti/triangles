% Receives a list of rectangles in the Cartesian plane and 
% prints the number of intersections between them and their pairs
% Use this to run the program: swipl -q -f triangles.pl -t topo < teste

% data
:- dynamic ret/5.

topo :-
    read(Input),       % Reads the input
    record(Input),
    write_ln(Input).

record([]).
record([X|R]) :- assertz(X), record(R).

% draft functor, not working
inrange(X11, X12, Y11, Y12, X21, X22, Y21, Y22) :- (X11 >= X21, Y11 >= Y21), (X12 =< X22, Y12 =< Y22).