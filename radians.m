%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: radians.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:59:19 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.radians.

:- interface.

:- import_module si_units.dim.
:- import_module si_units.dimmed_value.

%----------------------------------------------------------------------------%

:- type rad ---> rad(scale).

:- instance dimmed_value(rad).

:- func rad    = rad.
:- func turn   = rad.
:- func degree = rad.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module generic_math.
:- use_module math.
:- import_module maybe.

%----------------------------------------------------------------------------%

:- instance dimmed_value(rad) where [
    (dim(_) = one),
    (scale(rad(Scale)) = Scale),
    (symbol(_) = yes("rad"))
].
%----------------------------------------------------------------------------%

rad    = rad(1.0).
turn   = rad(2.0 * math.pi).
degree = rad(math.pi / 180.0).

%----------------------------------------------------------------------------%
:- end_module si_units.radians.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
