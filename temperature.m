%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: temperature.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:47:00 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.temperature.

:- interface.

:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- func kelvin `with_type` si_const `with_inst` si_const(temperature).
:- func 'K'    `with_type` si_const `with_inst` si_const(temperature).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

kelvin = unit(temperature).
'K' = kelvin.

%----------------------------------------------------------------------------%
:- end_module si_units.temperature.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
