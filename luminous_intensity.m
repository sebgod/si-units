%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: luminous_intensity.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 17:00:37 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.luminous_intensity.

:- interface.

:- import_module si_units.dim.

%----------------------------------------------------------------------------%

:- func candela `with_type` si_const `with_inst` si_const.
:- func cd      `with_type` si_const `with_inst` si_const.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

candela = unit(luminous_intensity).
cd = candela.

%----------------------------------------------------------------------------%
:- end_module si_units.luminous_intensity.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
