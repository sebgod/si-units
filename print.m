%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: print.m
% Copyright (C) 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:03:06 CEST 2014
%
% Pretty print routines to nicely format all SI Units and derived units
%

:- module si_units.print.

:- interface.

:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module si_units.dim.
:- import_module si_units.dimmed_value.
:- import_module si_units.scalar.

%----------------------------------------------------------------------------%
%
% Helper types for adding formatting descriptors
%

    % update_formatters/3 will use the get_default_formatter_map,
    % add the fmts list to the formatter_map sequentially,
    % and call set_default_formatter_map with the new map.
:- pred update_formatters(fmts::in(fmts), io::di, io::uo) is det.

:- type fmts == list(fmt).

:- inst fmts == list_skel(fmt).

:- type fmt
    ---> fmt(
            fmt_module    :: string,    % FQN Module name
            fmt_name      :: string,    % Type name
            fmt_arity     :: int,       % Type arity
            fmt_formatter :: formatter  % pretty_printer function
         ).

:- inst fmt
    ---> fmt(ground, ground, ground, formatter_func).

:- inst formatter_func == (func(in, in) = out is det).

:- func fmt_any((func(T) = doc)) `with_type` formatter.

:- pred set_formatter_sv(fmt::in, formatter_map::in, formatter_map::out)
    is det.

%----------------------------------------------------------------------------%
%
% Formatting functions for dimensions
%

:- func si_unit_symbol(base_quantity) = string.

:- func dimmed_value_to_doc(dimmed_value) = doc.
:- func (any_dimmed_value_to_doc(T) = doc) <= dimmed_value(T).

:- func scalar_to_doc(scalar) = doc.

:- func dim_to_doc(dim) = doc.

:- func exp_to_doc(exp) = doc.

:- func empty_str = doc.

:- func space = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module deconstruct.
:- import_module generic_math.
:- use_module int.
:- use_module integer.
:- import_module maybe.
:- use_module rational.
:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

%----------------------------------------------------------------------------%

dimmed_value_to_doc(DimmedValue) =
    any_dimmed_value_to_doc(DimmedValue).

any_dimmed_value_to_doc(DimmedValue) = group(FmtDim) :-
    Scale = scale(DimmedValue),
    Dim   = dim(DimmedValue),
    CustomSymbol = symbol(DimmedValue),
    FmtDim =
    ( Scale = one, Dim \= one ->
        [( if CustomSymbol = yes(S) then str(S) else dim_to_doc(Dim) )]
    ;
        [scalar_to_doc(Scale)] ++
        ( CustomSymbol = yes(S) ->
            [space, str(S)]
        ; Dim = one ->
            [empty_str]
        ;
            [space, dim_to_doc(Dim)]
        )
    ).

scalar_to_doc(scalar(V, P)) =
    str(string.format("%0." ++ from_int(P) ++ "e", [f(V)])).

dim_to_doc(Dim) =
    ( Dim = one ->
        str("")
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
    ;
        docs([str("⁽"), str(integer_to_sup_str(Numer)),
              str("⁄"), str(integer_to_sub_str(Denom)),  str("₎")])
    ).

:- func summand_to_univ(scalar, dim) = univ.

summand_to_univ(Scale, Dim) = univ(dimmed_value(Scale, Dim, no)).

:- func map_to_univ(list(T)) = list(univ).

map_to_univ(List) = map((func(E) = univ(E)), List).

:- func integer_to_sup_str(integer.integer) = string.

integer_to_sup_str(Integer) = integer_filter_map_str(digit_to_sup, Integer).

:- func integer_to_sub_str(integer.integer) = string.

integer_to_sub_str(Integer) = integer_filter_map_str(digit_to_sub, Integer).

:- inst filter_func == (func(in) = out is semidet).
:- func integer_filter_map_str((func(char) = char), integer.integer) = string.
:- mode integer_filter_map_str(in(filter_func), in) = out is det.

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

empty_str = str("").

space = str(" ").

%----------------------------------------------------------------------------%
% Formatter update API
%

update_formatters(Fmts, !IO) :-
    get_default_formatter_map(FMap0, !IO),
    foldl(set_formatter_sv, Fmts, FMap0, FMap),
    set_default_formatter_map(FMap, !IO).

set_formatter_sv(Fmt, !FMap) :-
    !:FMap = set_formatter(Fmt^fmt_module, Fmt^fmt_name,
                         Fmt^fmt_arity, Fmt^fmt_formatter, !.FMap).

fmt_any(Fun, Univ, _Args) =
    ( Univ = univ(X) ->
        Fun(X)
    ;
        unexpected($file, $module, "cannot print") % TODO deconstruct Univ
    ).

%----------------------------------------------------------------------------%
% Initialisation code for pretty printing
%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters(
        [
            fmt("si_units.scalar", "scalar", 0, fmt_any(scalar_to_doc)),
            fmt("si_units.dim", "dim", 0, fmt_any(dim_to_doc)),
            fmt("si_units.dimmed_value", "dimmed_value", 1,
                fmt_any(dimmed_value_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module si_units.print.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
