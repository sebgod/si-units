%------------------------------------------------------------------------------%
% File: test_si_units.m
% vim: ft=mercury ff=unix ts=4 sw=4 et
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun 11 May 20:59:29 CEST 2014
%
%------------------------------------------------------------------------------%

:- module test_si_units.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

:- import_module si_units.
:- import_module pretty_printer.

:- type rect
    ---> rect(dimmed_value, dimmed_value).
:- inst rect
    ---> rect(metre, metre).

:- func area(rect) = dimmed_value.
%:- func area(rect::in(rect)) = (dimmed_value::out(dimmed_value)) is det.

area(rect(A, B)) = A * B.

main(!IO) :-
    print_test("Unit of length", metre, !IO),
    print_test("4m + 6m", 4.0*m + 6.0*m, !IO),
    print_test("30s - 2s", 30.0*s - 2.0*s, !IO),
    print_test("celsius", 273.15 + kelvin, !IO),
    print_test("Area rect(2m, 3m)", area(rect(2.0*m, 3.0*m)), !IO),
    print_test("Velocity", m/s, !IO),
    print_test("Acceleration", m/(s**2), !IO),
    print_test("Hertz", 1.0/s, !IO),
    print_test("nano metres", m `exp` -9, !IO),
    print_test("millimetres", 1.0e-3*m, !IO),
    print_test("cube metres", m*m*m, !IO),
    print_test("AU", 'AU', !IO),
    print_test("lightyear", ly, !IO).

:- pred print_test(string::in, T::in, io::di, io::uo) is det.

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    write_doc(format(Entity), !IO),
    io.nl(!IO).

%------------------------------------------------------------------------------%
:- end_module test_si_units.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%------------------------------------------------------------------------------%
