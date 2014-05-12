%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: si_units.length.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 14:08:00 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.length.

:- interface.

:- import_module si_units.dim.
:- import_module si_units.dimmed_value.

%----------------------------------------------------------------------------%

:- type metre ---> metre(scale).

:- instance dimmed_value(metre).

:- func metre = metre.
:- func m     = metre.

:- func astronomical_unit = dimmed_value.
:- func 'AU'      = dimmed_value.

:- func lightyear = dimmed_value.
:- func ly        = dimmed_value.

:- func parsec = dimmed_value.
:- func pc     = dimmed_value.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- instance dimmed_value(metre) where [
    (dim(_) = unit(length)),
    (scale(metre(Scale)) = Scale)
].

%----------------------------------------------------------------------------%

metre = metre(1.0).
m = metre.

astronomical_unit = 149597870700.0 * m.
'AU' = astronomical_unit.

lightyear = 9.4605284e15 * m.
ly = lightyear.

parsec = 3.0857e16 * m.
pc = parsec.

%----------------------------------------------------------------------------%
:- end_module si_units.length.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
