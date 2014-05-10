%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
% File: si_units.m
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Copyright (C): 2014
% Created on: Sat 10 May 11:00:45 CEST 2014
%
%------------------------------------------------------------------------------%

:- module si_units.

:- interface.

:- import_module io.
:- import_module list.
:- import_module generic_math.

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amound_of_substance.

%:- type dimmed_value(TD, TV)
%    --->    dimmed_value(
%                        value :: TV,
%                        dimension :: TD
%            ).

% :- type dimmed_value == dimmed_value(float, dim).


:- typeclass dim(T) where [
    func dim(T) = dim,
    (some [TV] func value(T) = TV => generic_math(TV))
].

:- instance dim(dim).
:- instance dim(list(T)) <= dim(T).

:- type dim
    ---> base(base_quantity)
    ;    product(list(dim))
    ;    power(dim, int).

:- inst dim
    ---> base(ground)
    ;    product(list_skel(dim))
    ;    power(dim, ground)
    .


:- func m = dim.
:- func metre = dim.
% :- func 'AU' = dimmed_value.

:- func s = dim.
:- func second = dim.

:- func kg = dim.
:- func kilogram = dim.

:- func 'K' = dim.
:- func kelvin = dim.

:- func T ** int = dim <= dim(T).

:- func T1 * T2  = dim <= (dim(T1), dim(T2)).

:- func T1 / T2  = dim <= (dim(T1), dim(T2)).

% :- func exp(dim, int) = dim.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- use_module math.

:- instance dim(dim) where [
    (dim(Dim) = Dim),
    (value(_) = 1.0)
].

:- instance dim(list(T)) <= dim(T) where [
    (dim(List) = product(map(dim, List))),
    (value(_)  = 1.0)
].

Dim ** Exp =
    ( dim(Dim) = base(_) ->
        power(dim(Dim), Exp)
    ; dim(Dim) = power(Base, Exp0) ->
        power(Base, Exp0 * Exp)
    ;
        unexpected($file, $pred, "not implemented yet")
    ).

Multiplicand * Multiplier = Product :-
    Md = dim(Multiplicand),
    Mr = dim(Multiplier),
    Product =
    (
        Md = base(Base1)
    ->
        ( Mr = base(Base2) ->
            ( Base1 = Base2 ->
                power(Md, 2)
            ;
                product([Md, Mr])
            )
        ; Mr = product(Prod1) ->
            product([Md] ++ Prod1)
        ;
            product([Md] ++ [Mr])
        )
    ;
        Md = power(Base1, Exp1)
    ->
        ( Mr = power(Base2, Exp2) ->
            ( Base1 = Base2 ->
                power(Base1, Exp1 * Exp2)
            ;
                product([Md, Mr])
            )
        ; Mr = product(Prod1) ->
            product([Md] ++ Prod1)
        ; Mr = Base1 ->
            power(Base1, Exp1 + 1)
        ;
            product([Md] ++ [Mr])
        )
    ;
        Md = product(Prod1)
    ->
        ( Mr = product(Prod2) ->
            product(Prod1 ++ Prod2)
        ;
            product(Prod1 ++ [Mr])
        )
    ;
        unexpected($file, $pred, "unsupported dimension product")
    ).

Divident / Divisor = dim(Divident * power(dim(Divisor), -1)).

% exp(Dim, Scale) = Dim-math.pow(10.0, to_float(Scale)).

m  = base(length).
metre  = base(length).
% 'AU' = base(length)-1495078707000.0.

s  = base(time).
second  = base(time).

kg = base(mass).
kilogram = base(mass).

'K' = base(temperature).
kelvin = base(temperature).

%------------------------------------------------------------------------------%

main(!IO) :-
    print_test("Velocity", m / s, !IO),
    print_test("Acceleration", m * s ** -2, !IO),
    print_test("Hertz", s ** -1, !IO),
   % print_test("nano metres", m `exp` -9, !IO),
    print_test("cube metres", m * m * m, !IO).

:- pred print_test(string::in, T::in, io::di, io::uo) is det.

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    io.write_line(Entity, !IO).

%------------------------------------------------------------------------------%
:- end_module si_units.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%------------------------------------------------------------------------------%
