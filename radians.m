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

%----------------------------------------------------------------------------%

:- func rad   `with_type` si_const `with_inst` si_const.
:- func turn   = dimmed_value.
:- func degree = dimmed_value.
:- func '°'    = dimmed_value.
:- func pi_rad = dimmed_value.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

pi_rad = math.pi * rad.

rad = one.
turn = 2.0 * pi_rad.
degree = math.pi / 180.0.
'°' = degree.

%----------------------------------------------------------------------------%
:- end_module si_units.radians.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
