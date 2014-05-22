%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: length.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 14:08:00 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.length.

:- interface.

:- import_module si_units.scalar.
:- import_module si_units.dimmed_value.

%----------------------------------------------------------------------------%

:- type metre(T) ---> m(T).

:- type metre == metre(float).

:- instance dimmed_value(metre(T)) <= scalar(T).

:- func metre = metre.
:- func m     = metre.

:- func astronomical_unit = metre.
:- func 'AU'      = metre.

:- func lightyear = metre.
:- func ly        = metre.

:- func parsec = metre.
:- func pc     = metre.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module si_units.dim.
:- import_module si_units.print.
:- import_module univ.

%----------------------------------------------------------------------------%

:- instance dimmed_value(metre(T)) <= scalar(T) where [
    (dim(_) = unit(length)),
    (scale(m(Scale)) = scalar(Scale)),
    (symbol(_) = no)
].

%----------------------------------------------------------------------------%

metre = m(1.0).
m = metre.

astronomical_unit = m(149597870700.0).
'AU' = astronomical_unit.

lightyear = m(9.4605284e15).
ly = lightyear.

parsec = m(3.0857e16).
pc = parsec.

%----------------------------------------------------------------------------%
%
% Initialise formatter for metre
%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters([fmt($module, "metre", 1, fmt_any(metre_to_doc))], !IO).

:- func metre_to_doc(metre) = doc.

metre_to_doc(Metre) = any_dimmed_value_to_doc(Metre).

%----------------------------------------------------------------------------%
:- end_module si_units.length.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
