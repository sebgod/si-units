%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: electric_current.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:51:00 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.electric_current.

:- interface.

:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- func ampere `with_type` si_const `with_inst` si_const.

:- func 'A'    `with_type` si_const `with_inst` si_const.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

ampere = unit(electric_current).
'A' = ampere.

%----------------------------------------------------------------------------%
:- end_module si_units.electric_current.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
