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
:- use_module rational.

:- type scale == float.  % TODO somehow use generic math type
:- type exp == rational.rational.

:- typeclass dimmed_value(T) where [
    func dim(T) = dim,
    func scale(T) = scale
].

:- instance dimmed_value(dim).
:- instance dimmed_value(list(T)) <= dimmed_value(T).
:- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T).
:- instance dimmed_value(scale).

:- type dim
    ---> one
    ;    unit(base_quantity)
    ;    product(list(dim))
    ;    power(dim, exp).

:- inst dim
    ---> one
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

:- inst dimmed_value
    ---> dimmed_value(unique, dim).

:- inst metre ---> dimmed_value(ground, bound(length)).

%------------------------------------------------------------------------------%

:- type si_const == ((func) = dim).

:- inst si_const == ((func) = (out(dim)) is det).
:- inst si_derived == ((func) = (out(dimmed_value)) is det).

:- func metre `with_type` si_const `with_inst` si_const.
:- func m `with_type` si_const `with_inst` si_const.
:- func 'AU' = dimmed_value.
:- func lightyear = dimmed_value.
:- func ly = dimmed_value.

:- func second = (dim::out(dim)) is det.
:- func s = (dim::out(dim)) is det.

:- func kilogram = (dim::out(dim)) is det.
:- func kg = (dim::out(dim)) is det.

:- func kelvin = dim.
:- func 'K' = dim.

:- func candela = dim.
:- func cd = dim.

:- func mole = dim.
:- func mol = dim.

:- func ampere = dim.
:- func 'A' = dim.

:- func T ** TExp = dimmed_value
    <= (scalar_generic_math(TExp), dimmed_value(T)).
%:- mode (in(dim) ** in(unique)) = out(dimmed_value) is det.

:- func T1 * T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).
%:- mode (in(dim) * in(dimmed_value)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(dim)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(unique)) = out(dimmed_value) is det.
%:- mode (in(unique) * in(dimmed_value)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(dimmed_value)) = out(dimmed_value) is det.

:- func T1 / T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func exp(T, int) = dimmed_value <= dimmed_value(T).

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
    (dim(_) = one),
    (scale(Scale) = Scale)
].

Base ** Exp = dimmed_value(scale(Dim), Dim) :-
    Dim0 = dim(Base),
    Exp1 = to_rational(Exp),
    Dim =
    ( Dim0 = unit(_) ->
        power(Dim0, Exp1)
    ; Dim0 = power(BaseUnit0, Exp0) ->
        power(BaseUnit0, Exp0 * Exp1)
    ;
        unexpected($file, $pred, "not implemented yet")
    ).

Multiplicand * Multiplier = dimmed_value(Scale, Dim) :-
    Scale = scale(Multiplicand) * scale(Multiplier),
    Dim   = times(dim(Multiplicand), dim(Multiplier)).

:- func times(dim, dim) = dim.

times(Md, Mr) =
    (
        Md = one
    ->
        Mr
    ;
        Mr = one
    ->
        Md
    ;
        Md = unit(Base1)
    ->
        ( Mr = unit(Base2) ->
            ( Base1 = Base2 ->
                power(Md, to_rational(2))
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
            power(Base1, Exp1 + rational.one)
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
    Dim = dim(Divident) `times` power(dim(Divisor), to_rational(-1)).

exp(Value, Exp) = dimmed_value(Scale, dim(Value)) :-
    Scale = scale(Value) * 10.0 ** to_float(Exp).

metre = unit(length).
m = metre.
'AU' = 149597870700.0 * m.
lightyear = 9.4605284e15 * m.
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

:- type rect
    ---> rect(dimmed_value, dimmed_value).
:- inst rect
    ---> rect(metre, metre).

:- func area(rect) = dimmed_value.
%:- func area(rect::in(rect)) = (dimmed_value::out(dimmed_value)) is det.

area(rect(A, B)) = A * B.

main(!IO) :-
    print_test("Area rect(2m, 3m)", area(rect(2.0*m, 3.0*m)), !IO),
    print_test("Velocity", m/s, !IO),
    print_test("Acceleration", m/s**2, !IO),
    print_test("Hertz", 1.0/s, !IO),
    print_test("nano metres", m `exp` -9, !IO),
    print_test("millimetres", 1.0e-3*m, !IO),
    print_test("cube metres", m*m*m, !IO),
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
