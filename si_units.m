#!/usr/bin/env mmi

:- type dim
    ---> length
    ;    time
    ;    list(dim)
    .

m = dim(length).

main :-
    io.write_line(m, !IO).
    