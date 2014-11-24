%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: amount_of_substance.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:57:49 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.amount_of_substance.

:- interface.

:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- func mole  `with_type` si_const `with_inst` si_const.
:- func mol   `with_type` si_const `with_inst` si_const.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

mole = unit(amount_of_substance).
mol = mole.

%----------------------------------------------------------------------------%
:- end_module si_units.amount_of_substance.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
