%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: dim.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:27:26 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.dim.

:- interface.

:- import_module list.
:- use_module rational.
:- import_module si_units.scalar.

%----------------------------------------------------------------------------%

:- type exp == rational.rational.

:- type dim
    ---> one
    ;    unit(base_quantity)
    ;    sum(list(scalar), list(dim))
    ;    product(list(dim))
    ;    square(dim)
    ;    cube(dim)
    ;    power(dim, exp).

:- inst dim
    ---> one
    ;    unit(ground)
    ;    sum(list_skel(ground), list_skel(dim))
    ;    product(list_skel(dim))
    ;    square(dim)
    ;    cube(dim)
    ;    power(dim, ground).

:- inst unit(I) == bound(unit(I)).
:- inst squared_unit(I) ---> square(unit(I)).

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amount_of_substance.

:- func norm(dim) = dim is det.

:- func times(dim, dim) = dim.

:- type si_const == ((func) = dim).
:- inst si_const == ((func) = (out(dim)) is det).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module generic_math.
:- import_module require.

%----------------------------------------------------------------------------%

norm(Dim) =
    ( Dim = square(Base) ->
        power(Base, rational.rational(2))
    ; Dim = cube(Base) ->
        power(Base, rational.rational(3))
    ;
        Dim
    ).

times(Md0, Mr0) = Product :-
    Md = norm(Md0),
    Mr = norm(Mr0),
    Product =
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
                square(Md)
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
                power(Base1, Exp1 + Exp2)
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
        unexpected($file, $pred, "unsupported × dimension product")
    ).



%----------------------------------------------------------------------------%
:- end_module si_units.dim.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
