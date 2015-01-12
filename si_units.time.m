%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: si_units.time.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 15:32:54 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.time.

:- interface.

:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- func second `with_type` si_const `with_inst` si_const(time).
:- func s      `with_type` si_const `with_inst` si_const(time).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

second  = unit(time).
s = second.

%----------------------------------------------------------------------------%
:- end_module si_units.time.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
