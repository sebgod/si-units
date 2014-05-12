%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: si_units.dimmed_value.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:31:20 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.dimmed_value.

:- interface.

:- import_module list.
:- import_module generic_math.
:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- typeclass dimmed_value(T) where [
    func dim(T) = dim,
    func scale(T) = scale
].

:- instance dimmed_value(dim).
:- instance dimmed_value(list(T)) <= dimmed_value(T).
:- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T).
:- instance dimmed_value(scale).

:- type dimmed_value(T) ---> dimmed_value(scale, T).

:- type dimmed_value == dimmed_value(dim).

:- inst dimmed_value
    ---> dimmed_value(unique, dim).

%----------------------------------------------------------------------------%
% Binary operators for a dimmed_value
%

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

:- func T1 + T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func T1 - T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func exp(T, int) = dimmed_value <= dimmed_value(T).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- use_module math.
:- use_module rational.

%----------------------------------------------------------------------------%
% Type class instances
%

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

%----------------------------------------------------------------------------%

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

%----------------------------------------------------------------------------%

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

Divident / Divisor = Divident * Divisor ** (-1).

%----------------------------------------------------------------------------%

Augend + Addend = Sum :-
    DimAu = dim(Augend),
    DimAd = dim(Addend),
    ScaleAu = scale(Augend),
    ScaleAd = scale(Addend),
    ( DimAu = DimAd ->
        Sum = dimmed_value(ScaleAu + ScaleAd, DimAu)
    ;
        Sum = dimmed_value(1.0, sum([ScaleAu, ScaleAd], [DimAu, DimAd]))
    ).

Minuend - Subtrahend = Minuend + (-1.0 * Subtrahend ).

exp(Value, Exp) = dimmed_value(Scale, dim(Value)) :-
    Scale = scale(Value) * 10.0 ** to_float(Exp).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
:- end_module si_units.dimmed_value.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
