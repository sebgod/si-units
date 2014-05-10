%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
% File: si_units.m
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Copyright (C) 2014 Sebastian Godelet
% Created on: Sat 10 May 11:00:45 CEST 2014
%
%------------------------------------------------------------------------------%

:- module si_units.

:- interface.

:- import_module io.
:- import_module list.
:- import_module generic_math.

:- type scale == float.

:- typeclass dimmed_value(T) where [
    func dim(T) = dim,
    func scale(T) = scale
].

:- instance dimmed_value(dim).
:- instance dimmed_value(list(T)) <= dimmed_value(T).
:- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T).
:- instance dimmed_value(scale).

:- type dim
    ---> zero
    ;    unit(base_quantity)
    ;    product(list(dim))
    ;    power(dim, int).

:- inst dim
    ---> zero
    ;    unit(ground)
    ;    product(list_skel(dim))
    ;    power(dim, ground).

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amount_of_substance.

:- type dimmed_value(T) ---> dimmed_value(scale, T).

:- type dimmed_value == dimmed_value(dim).

%------------------------------------------------------------------------------%

:- func metre = dim.
:- func m = dim.
:- func 'AU' = dimmed_value.
:- func lightyear = dimmed_value.
:- func ly = dimmed_value.

:- func second = dim.
:- func s = dim.

:- func kilogram = dim.
:- func kg = dim.

:- func kelvin = dim.
:- func 'K' = dim.

:- func candela = dim.
:- func cd = dim.

:- func mole = dim.
:- func mol = dim.

:- func ampere = dim.
:- func 'A' = dim.

:- func T ** int = dimmed_value <= dimmed_value(T).

:- func T1 * T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func T1 / T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func exp(T, scale) = dimmed_value <= dimmed_value(T).

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.

 :- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T) where [
    (dim(dimmed_value(_Scale, Dim)) = dim(Dim)),
    (scale(dimmed_value(Scale, _Dim)) = Scale)
].

:- instance dimmed_value(dim) where [
    (dim(Dim) = Dim),
    (scale(_) = 1.0)
].

:- instance dimmed_value(list(T)) <= dimmed_value(T) where [
    (dim(List) = product(map(dim, List))),
    (scale(_)  = 1.0)
].

:- instance dimmed_value(scale) where [
    (dim(_) = zero),
    (scale(Scale) = Scale)
].

Base ** Exp = dimmed_value(scale(Dim), Dim) :-
    Dim0 = dim(Base),
    Dim =
    ( Dim0 = unit(_) ->
        power(Dim0, Exp)
    ; Dim0 = power(BaseUnit0, Exp0) ->
        power(BaseUnit0, Exp0 * Exp)
    ;
        unexpected($file, $pred, "not implemented yet")
    ).

Multiplicand * Multiplier = dimmed_value(Scale, Dim) :-
    Scale = scale(Multiplicand) * scale(Multiplier),
    Dim   = times(dim(Multiplicand), dim(Multiplier)).

:- func times(dim, dim) = dim.

times(Md, Mr) =
    (
        Md = zero
    ->
        Mr
    ;
        Mr = zero
    ->
        Md
    ;
        Md = unit(Base1)
    ->
        ( Mr = unit(Base2) ->
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

Divident / Divisor = dimmed_value(Scale, Dim) :-
    Scale = divide(scale(Divident), scale(Divisor)),
    Dim = dim(Divident) `times` power(dim(Divisor), -1).

exp(Value, Exp) = dimmed_value(Scale, dim(Value)) :-
    Scale = scale(Value) * 10.0 ** Exp.

metre = unit(length).
m = metre.
'AU' = dimmed_value(149597870700.0, m).
lightyear = dimmed_value(9.4605284e15, m).
ly = lightyear.

second  = unit(time).
s = second.

kilogram = unit(mass).
kg = kilogram.

kelvin = unit(temperature).
'K' = kelvin.

candela = unit(luminous_intensity).
cd = candela.

mole = unit(amount_of_substance).
mol = mole.

ampere = unit(electric_current).
'A' = ampere.

%------------------------------------------------------------------------------%

main(!IO) :-
    print_test("Velocity", m/s, !IO),
    print_test("Acceleration", m/s**2, !IO),
    print_test("Hertz", 1.0/s, !IO),
    print_test("nano metres", m `exp` -9.0, !IO),
    print_test("millimetres", 1.0e-3 * m, !IO),
    print_test("cube metres", m * m * m, !IO),
    print_test("AU", 'AU', !IO),
    print_test("lightyear", ly, !IO).

:- pred print_test(string::in, T::in, io::di, io::uo) is det.

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    io.write_line(Entity, !IO).

%------------------------------------------------------------------------------%
:- end_module si_units.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%------------------------------------------------------------------------------%
