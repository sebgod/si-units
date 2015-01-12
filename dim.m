%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: dim.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:27:26 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.dim.

:- interface.

:- import_module list.
:- import_module si_units.exp.
:- import_module si_units.scalar.

%----------------------------------------------------------------------------%

:- type dim
    ---> one
    ;    unit(base_quantity)
    ;    sum(list(scalar), list(dim))
    ;    product(list(dim))
    ;    power(dim, exp).

:- inst dim
    ---> one
    ;    unit(ground)
    ;    sum(list_skel(ground), list_skel(dim))
    ;    product(list_skel(dim))
    ;    power(dim, ground).

:- inst one ---> one.
:- inst unit == unit(ground).
:- inst power == bound(power(dim, ground)).

:- inst squared_unit(I) == powered_unit(I, two).
:- inst cubed_unit(I)   == powered_unit(I, three).
:- inst unit(I)         == bound(unit(I)).
:- inst powered_unit(I, E) ---> power(unit(I), E).

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amount_of_substance.

:- inst base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amount_of_substance.

:- inst time ---> time.
:- inst mass ---> mass.
:- inst temperature ---> temperature.
:- inst length ---> length.
:- inst electric_current ---> electric_current.
:- inst luminous_intensity ---> luminous_intensity.
:- inst amount_of_substance ---> amount_of_substance.

:- func times(dim, dim) = dim.
:- mode times(in, in) = out is det.
%:- mode times(in(one), in(I)) = out(I) is det.

:- type si_const == ((func) = dim).
:- inst si_const(I) == ((func) = (out(unit(I))) is det).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module generic_math.
:- import_module require.

%----------------------------------------------------------------------------%

times(Md, Mr) = Product :-
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
                power(Md, two)
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
            power(Base1, Exp1 + one)
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
%----------------------------------------------------------------------------%
