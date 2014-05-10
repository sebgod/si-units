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
:- import_module pair.
:- import_module generic_math.

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amound_of_substance.

:- typeclass dim(T) where [
    func dim(T) = dim
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

:- type scaled_dim == pair(dim, int).

:- type dimmed_value
   ---> some [T]
    (
        dimmed_value(
                    T,
                    dim
        ) => generic_math(T)
    ).

:- func m = dim.
:- func metre = dim.

:- func s = dim.
:- func second = dim.

:- func kg = dim.
:- func kilogram = dim.

:- func 'K' = dim.
:- func kelvin = dim.

:- func dim ** int = dim.

:- func dim *  dim = dim.

:- func dim /  dim = dim.

:- func exp(dim, int) = scaled_dim.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- use_module math.

:- instance dim(dim) where [
    (dim(Dim) = Dim)
].

:- instance dim(list(T)) <= dim(T) where [
    (dim(List) = product(map(dim, List)))
].

Dim ** Exp =
    ( Dim = base(_) ->
        power(Dim, Exp)
    ; Dim = power(Base, Exp0) ->
        power(Base, Exp0 * Exp)
    ;
        unexpected($file, $pred, "not implemented yet")
    ).

Multiplicand * Multiplier =
    (
        Multiplicand = base(Base1)
    ->
        ( Multiplier = base(Base2) ->
            ( Base1 = Base2 ->
                power(Multiplicand, 2)
            ;
                product([Multiplicand, Multiplier])
            )
        ; Multiplier = product(Prod1) ->
            product([Multiplicand] ++ Prod1)
        ;
            product([Multiplicand] ++ [Multiplier])
        )
    ;
        Multiplicand = power(Base1, Exp1)
    ->
        ( Multiplier   = power(Base2, Exp2) ->
            ( Base1 = Base2 ->
                power(Base1, Exp1 * Exp2)
            ;
                product([Multiplicand, Multiplier])
            )
        ; Multiplier = product(Prod1) ->
            product([Multiplicand] ++ Prod1)
        ; Multiplier = Base1 ->
            power(Base1, Exp1 + 1)
        ;
            product([Multiplicand] ++ [Multiplier])
        )
    ;
        Multiplicand = product(Prod1)
    ->
        ( Multiplier = product(Prod2) ->
            product(Prod1 ++ Prod2)
        ;
            product(Prod1 ++ [Multiplier])
        )
    ;
        unexpected($file, $pred, "unsupported dimension product")
    ).

Divident / Divisor = Divident * power(Divisor, -1).

exp(Dim, Scale) = Dim-Scale.

m  = base(length).
metre  = base(length).

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
    print_test("nano metres", m `exp` -9, !IO),
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
