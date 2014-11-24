%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: scalar.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Wed May 14 13:41:34 CEST 2014
%
%----------------------------------------------------------------------------%

:- module si_units.scalar.

:- interface.

:- import_module float.
:- import_module generic_math.

%----------------------------------------------------------------------------%

:- typeclass scalar(T) where [
    func scalar(T) = scalar
].

:- instance generic_math(scalar).
:- instance scalar_generic_math(scalar).

:- instance scalar(scalar).
:- instance scalar(float).

:- type scalar --->
    scalar(
        fws_value   :: float,  % the floating point value
        fws_res     :: int     % the precision
    ).

:- func zero = scalar.
:- func one = scalar.
:- func max_resolution = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- use_module int.
:- use_module math.
:- use_module std_util.
:- import_module string.

%----------------------------------------------------------------------------%
%
% Identities
%

zero = scalar(0.0, max_resolution).

one = scalar(1.0, max_resolution).

max_resolution = 8.

%----------------------------------------------------------------------------%
%
% Instance definitions for typeclass generic_math(T)
%

:- instance generic_math(scalar) where [
    (abs(scalar(V, R)) = scalar(float.abs(V), R)),

    (min(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.min(V1, V2), int.min(R1, R2))),

    (max(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.max(V1, V2), int.min(R1, R2))),

    (times(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.'*'(V1, V2), int.min(R1, R2))),

    (divide(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.'/'(V1, V2), int.min(R1, R2))),

    (pow(scalar(V1, R1), scalar(V2, R2)) =
        scalar(math.pow(V1, V2), int.min(R1, R2))),

    (add(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.'+'(V1, V2), int.min(R1, R2))),

    (substract(scalar(V1, R1), scalar(V2, R2)) =
        scalar(float.'-'(V1, V2), int.min(R1, R2)))
].

%----------------------------------------------------------------------------%
%
% Instance definitions for typeclass scalar_generic_math(T)
%

:- instance scalar_generic_math(scalar) where [
    (times_float(scalar(V, _R), Float) = float.'*'(V, Float)),

    (to_int(_) =
        throw(math.domain_error($pred ++ ": cannot cast to int"))),

    (to_integer(_Float) =
        throw(math.domain_error($pred ++ ": cannot cast to int"))),

    (to_float(scalar(V, _)) = V),

    (to_rational(_F) =
        throw(math.domain_error($pred ++ ": cannot cast to rational")))
].

%----------------------------------------------------------------------------%
%
% Instance definitions for typeclass scalar(T)
%

:- instance scalar(scalar) where [
    ((func scalar/1) is std_util.id)
].

:- instance scalar(float) where [
    (scalar(V) = scalar(V, max_resolution))
].

%----------------------------------------------------------------------------%
:- end_module si_units.scalar.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
