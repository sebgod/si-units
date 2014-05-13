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

:- import_module io.
:- import_module list.
:- import_module generic_math.
:- use_module math.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module si_units.print.
:- import_module univ.

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
%
% Initialise formatter for radians
%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters([fmt($module, "rad", 0, fmt_radians)], !IO).

:- func fmt_radians `with_type` formatter `with_inst` formatter_func.

fmt_radians(Univ, _Args) =
    ( Univ = univ(X) ->
        radians_to_doc(X)
    ;
        str("?radians?")
    ).

:- func radians_to_doc(rad) = doc.

radians_to_doc(Radians) = any_dimmed_value_to_doc(Radians).

%----------------------------------------------------------------------------%
:- end_module si_units.radians.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
