%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: print.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:03:06 CEST 2014
%
% Pretty print routines to nicely format all SI Units and derived units
%----------------------------------------------------------------------------%

:- module si_units.print.

:- interface.

:- import_module si_units.dim.
:- import_module si_units.dimmed_value.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- func si_unit_symbol(base_quantity) = string.

:- func dimmed_value_to_doc(dimmed_value) = doc.

:- func dim_to_doc(dim) = doc.

:- func exp_to_doc(exp) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module deconstruct.
:- import_module generic_math.
:- use_module integer.
:- import_module io.
:- import_module list.
:- use_module rational.
:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

%----------------------------------------------------------------------------%

dimmed_value_to_doc(dimmed_value(Scale, Dim)) =
    ( if Scale = 1.0, Dim \= one then
        dim_to_doc(Dim)
      else
        group([format(Scale),
               str(if Dim \= one then " " else ""),
               dim_to_doc(Dim)])
    ).

dim_to_doc(Dim) =
    ( Dim = one ->
        str("") % maybe it would be nice to retain "rad", etc ...
    ; Dim = unit(Unit) ->
        str(si_unit_symbol(Unit))
    ; Dim = power(Base, Exp) ->
        docs([dim_to_doc(Base), exp_to_doc(Exp)])
    ; Dim = product(Product) ->
        format_list(map_to_univ(Product), str("\u2219"))
    ; Dim = sum(Scales, Dims) ->
        docs([str("("), format_list(
           map_corresponding(summand_to_univ, Scales, Dims), str(" + ")),
              str(")")])
    ;
        unexpected($file, $pred, "no pretty printer clause for dim")
    ).

si_unit_symbol(length) = "m".
si_unit_symbol(time) = "s".
si_unit_symbol(mass) = "kg".
si_unit_symbol(temperature) = "K".
si_unit_symbol(amount_of_substance) = "mol".
si_unit_symbol(electric_current) = "A".
si_unit_symbol(luminous_intensity) = "cd".

exp_to_doc(Exp) = Doc :-
    Numer = rational.numer(Exp),
    Denom = rational.denom(Exp),
    Doc =
    ( Denom = integer.one ->
        str(integer_to_sup_str(Numer))
    ; Denom = to_integer(2), Numer = integer.one ->
        str("½")
    ;
        docs([str("⁽"), str(integer_to_sup_str(Numer)),
              str("⁄"), str(integer_to_sub_str(Denom)),  str("₎")])
    ).

:- func summand_to_univ(scale, dim) = univ.

summand_to_univ(Scale, Dim) = univ(dimmed_value(Scale, Dim)).

:- func map_to_univ(list(T)) = list(univ).

map_to_univ(List) = map((func(E) = univ(E)), List).

:- func integer_to_sup_str(integer.integer) = string.

integer_to_sup_str(Integer) = integer_filter_map_str(digit_to_sup, Integer).

:- func integer_to_sub_str(integer.integer) = string.

integer_to_sub_str(Integer) = integer_filter_map_str(digit_to_sub, Integer).

:- func integer_filter_map_str((func(char) = char), integer.integer) = string.
:- mode integer_filter_map_str(in((func(in) = out) is semidet), in) = out is det.

integer_filter_map_str(Filter, Integer) =
    from_char_list(filter_map(Filter,
                    to_char_list(integer.to_string(Integer)))).


:- func digit_to_sup(char) = char is semidet.

digit_to_sup('0') = '⁰'.
digit_to_sup('1') = '¹'.
digit_to_sup('2') = '²'.
digit_to_sup('3') = '³'.
digit_to_sup('4') = '⁴'.
digit_to_sup('5') = '⁵'.
digit_to_sup('6') = '⁶'.
digit_to_sup('7') = '⁷'.
digit_to_sup('8') = '⁸'.
digit_to_sup('9') = '⁹'.
digit_to_sup('+') = '⁺'.
digit_to_sup('-') = '⁻'.

:- func digit_to_sub(char) = char is semidet.

digit_to_sub('0') = '₀'.
digit_to_sub('1') = '₁'.
digit_to_sub('2') = '₂'.
digit_to_sub('3') = '₃'.
digit_to_sub('4') = '₄'.
digit_to_sub('5') = '₅'.
digit_to_sub('6') = '₆'.
digit_to_sub('7') = '₇'.
digit_to_sub('8') = '₈'.
digit_to_sub('9') = '₉'.
digit_to_sub('+') = '₊'.
digit_to_sub('-') = '₋'.

%----------------------------------------------------------------------------%
% Initialisation code for pretty printing
%----------------------------------------------------------------------------%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    get_default_formatter_map(Fmt0, !IO),
    si_units.print.unit_formatter(Fmt0, Fmt),
    set_default_formatter_map(Fmt, !IO).

:- pred unit_formatter(formatter_map::in, formatter_map::out) is det.


unit_formatter(!Fmt) :-
    set_formatter_sv("si_units.dim", "dim", 0, fmt_dim, !Fmt),
    set_formatter_sv("si_units.dimmed_value", "dimmed_value", 1,
        fmt_dimmed_value, !Fmt).

:- func fmt_dim(univ, list(type_desc)) = doc.

fmt_dim(Univ, _Args) =
    ( if Univ = univ(X) then dim_to_doc(X) else  str("?dim?") ).

:- func fmt_dimmed_value(univ, list(type_desc)) = doc.

fmt_dimmed_value(Univ, _Args) =
    ( Univ = univ(X) ->
        dimmed_value_to_doc(X)
    ;
        str("?dimmed_value?")
    ).

:- pred set_formatter_sv(string::in, string::in, int::in, formatter::in,
    formatter_map::in, formatter_map::out) is det.

set_formatter_sv(ModuleName, TypeName, Arity, Formatter, FMap0, FMap) :-
    FMap = set_formatter(ModuleName, TypeName, Arity, Formatter, FMap0).

%----------------------------------------------------------------------------%
:- end_module si_units.print.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
