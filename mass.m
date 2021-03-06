%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: mass.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:45:33 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.mass.

:- interface.

:- import_module si_units.dim.

:- func kilogram `with_type` si_const `with_inst` si_const(mass).
:- func kg       `with_type` si_const `with_inst` si_const(mass).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

kilogram = unit(mass).
kg = kilogram.

%----------------------------------------------------------------------------%
:- end_module si_units.mass.
%----------------------------------------------------------------------------%
