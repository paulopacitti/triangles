% Receives a list of rectangles in the Cartesian plane and 
% prints the number of intersections between them and their pairs
% Use this to run the program: swipl -q -f triangles.pl -t topo < teste

topo():-
    read(Input),       % Reads the input
    write_ln(Input).
