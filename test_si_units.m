%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: test_si_units.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun 11 May 20:59:29 CEST 2014
%
%----------------------------------------------------------------------------%

:- module test_si_units.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- import_module generic_math.
:- import_module list.
:- use_module math.
:- import_module pretty_printer.
:- use_module rational.
:- import_module si_units.
:- import_module si_units.dim.
:- import_module si_units.dimmed_value.
:- import_module si_units.length.
:- import_module si_units.radians.
:- import_module si_units.scalar.
:- import_module si_units.temperature.
:- import_module si_units.time.

%----------------------------------------------------------------------------%

:- type rect   ---> rect(metre, metre).

:- inst rect   ---> rect(length, length).

:- inst area   == squared_unit(bound(length)).

:- func area(rect) = dimmed_value.
:- mode area(in) = out is det.
:- mode area(in(rect)) = out(area) is det.

area(rect(A, B)) = A * B.

main(!IO) :-
    print_test("Unit of length", metre, !IO),
    print_test("4 m + 6 m", 4.0*m + m(6.0), !IO),
    print_test("30 s - 2 s", 30.0*s - 2.0*s, !IO),
    print_test("[°C]", kelvin - 273.15, !IO),
    print_test("norm(m*m*m)", norm((m*m*m)^dim), !IO),
    print_test("norm(m**3)", norm((m**3)^dim), !IO),
    print_test("m² × m³", (m*m) * (m*m*m), !IO),
    print_test("Area rect(2m, 3m)", area(rect(m(2.0), m(3.0))), !IO),
    print_test("Velocity", m/s, !IO),
    print_test("Acceleration", m/(s**2), !IO),
    print_test("Hertz", 1.0/s, !IO),
    print_test("nano metres", m `exp` -9, !IO),
    print_test("millimetres", 1.0e-3*m, !IO),
    print_test("cube metres", m*m*m, !IO),
    print_test("AU", 'AU', !IO),
    print_test("lightyear", ly, !IO),
    print_test("sin(π/3.0)", math.sin(math.pi/3.0) * rad, !IO),
    print_test("tan(0.3 * π)", rad(math.tan(0.3 * math.pi)), !IO).

:- pred print_test(string::in, T::in, io::di, io::uo) is det
            <= dimmed_value(T).

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    write_doc(format(Entity), !IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%
:- end_module test_si_units.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
