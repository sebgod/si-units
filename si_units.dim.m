%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: si_units.dim.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:27:26 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.dim.

:- interface.

:- import_module list.
:- use_module rational.

%----------------------------------------------------------------------------%

:- type scale == float.  % TODO somehow use generic math type
:- type exp == rational.rational.

:- type dim
    ---> one
    ;    unit(base_quantity)
    ;    sum(list(scale), list(dim))
    ;    product(list(dim))
    ;    power(dim, exp).

:- inst dim
    ---> one
    ;    unit(ground)
    ;    sum(list_skel(ground), list_skel(dim))
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

:- type si_const == ((func) = dim).
:- inst si_const == ((func) = (out(dim)) is det).

%:- type si_derived == ((func) = dimmed_value).
%:- inst si_derived == ((func) = (out(dimmed_value)) is det).
% `with_type` si_derived `with_inst` si_derived..

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module si_units.dim.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
