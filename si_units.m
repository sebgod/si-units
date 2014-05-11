%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
% File: si_units.m
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Copyright (C) 2014 Sebastian Godelet
% Created on: Sat 10 May 11:00:45 CEST 2014
%
%------------------------------------------------------------------------------%

:- module si_units.

:- interface.

:- import_module io.
:- import_module list.
:- import_module generic_math.
:- import_module pretty_printer.
:- use_module rational.

:- type scale == float.  % TODO somehow use generic math type
:- type exp == rational.rational.

:- typeclass dimmed_value(T) where [
    func dim(T) = dim,
    func scale(T) = scale
].

:- instance dimmed_value(dim).
:- instance dimmed_value(list(T)) <= dimmed_value(T).
:- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T).
:- instance dimmed_value(scale).

:- type dim
    ---> one
    ;    unit(base_quantity)
    ;    sum(list(scale), list(dim))
    ;    product(list(dim))
    ;    power(dim, exp).

:- inst dim
    ---> one
    ;    unit(ground)
    ;    sum(list_skel(ground), list_skel(dim))
    ;    product(list_skel(dim))
    ;    power(dim, ground).

:- type base_quantity
    ---> time
    ;    mass
    ;    temperature
    ;    length
    ;    electric_current
    ;    luminous_intensity
    ;    amount_of_substance.

:- type dimmed_value(T) ---> dimmed_value(scale, T).

:- type dimmed_value == dimmed_value(dim).

:- inst dimmed_value
    ---> dimmed_value(unique, dim).

:- inst metre ---> dimmed_value(ground, bound(length)).

%------------------------------------------------------------------------------%

:- type si_const == ((func) = dim).
:- inst si_const == ((func) = (out(dim)) is det).

%:- type si_derived == ((func) = dimmed_value).
%:- inst si_derived == ((func) = (out(dimmed_value)) is det).

:- func metre `with_type` si_const `with_inst` si_const.
:- func m     `with_type` si_const `with_inst` si_const.

% `with_type` si_derived `with_inst` si_derived..
:- func astronomical_unit = dimmed_value.
:- func 'AU'      = dimmed_value.

:- func lightyear = dimmed_value.
:- func ly        = dimmed_value.

:- func parsec = dimmed_value.
:- func pc     = dimmed_value.

:- func second `with_type` si_const `with_inst` si_const.
:- func s      `with_type` si_const `with_inst` si_const.

:- func kilogram `with_type` si_const `with_inst` si_const.
:- func kg       `with_type` si_const `with_inst` si_const.

:- func kelvin `with_type` si_const `with_inst` si_const.
:- func 'K'    `with_type` si_const `with_inst` si_const.

:- func candela `with_type` si_const `with_inst` si_const.
:- func cd      `with_type` si_const `with_inst` si_const.

:- func mole  `with_type` si_const `with_inst` si_const.
:- func mol   `with_type` si_const `with_inst` si_const.

:- func ampere `with_type` si_const `with_inst` si_const.
:- func 'A'    `with_type` si_const `with_inst` si_const.

:- func rad   `with_type` si_const `with_inst` si_const.
:- func turn   = dimmed_value.
:- func degree = dimmed_value.
:- func '°'    = dimmed_value.

:- func T ** TExp = dimmed_value
    <= (scalar_generic_math(TExp), dimmed_value(T)).
%:- mode (in(dim) ** in(unique)) = out(dimmed_value) is det.

:- func T1 * T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).
%:- mode (in(dim) * in(dimmed_value)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(dim)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(unique)) = out(dimmed_value) is det.
%:- mode (in(unique) * in(dimmed_value)) = out(dimmed_value) is det.
%:- mode (in(dimmed_value) * in(dimmed_value)) = out(dimmed_value) is det.

:- func T1 / T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func T1 + T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func T1 - T2  = dimmed_value <= (dimmed_value(T1), dimmed_value(T2)).

:- func exp(T, int) = dimmed_value <= dimmed_value(T).

:- func si_unit_symbol(base_quantity) = string.

:- func dimmed_value_to_doc(dimmed_value) = doc.

:- func dim_to_doc(dim) = doc.

:- func exp_to_doc(exp) = doc.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- use_module math.
    % For pretty printing
:- import_module type_desc.
:- import_module univ.
:- import_module deconstruct.
:- import_module char.
:- import_module string.
:- use_module integer.

 :- instance dimmed_value(dimmed_value(T)) <= dimmed_value(T) where [
    (dim(dimmed_value(_Scale, Dim)) = dim(Dim)),
    (scale(dimmed_value(Scale, _Dim)) = Scale)
].

:- instance dimmed_value(dim) where [
    (dim(Dim) = Dim),
    (scale(_) = 1.0)
].

:- instance dimmed_value(list(T)) <= dimmed_value(T) where [
    (dim(List) = product(map(dim, List))),
    (scale(_)  = 1.0)
].

:- instance dimmed_value(scale) where [
    (dim(_) = one),
    (scale(Scale) = Scale)
].

%------------------------------------------------------------------------------%

Base ** Exp = dimmed_value(scale(Dim), Dim) :-
    Dim0 = dim(Base),
    Exp1 = to_rational(Exp),
    Dim =
    ( Dim0 = unit(_) ->
        power(Dim0, Exp1)
    ; Dim0 = power(BaseUnit0, Exp0) ->
        power(BaseUnit0, Exp0 * Exp1)
    ;
        unexpected($file, $pred, "not implemented yet")
    ).

Multiplicand * Multiplier = dimmed_value(Scale, Dim) :-
    Scale = scale(Multiplicand) * scale(Multiplier),
    Dim   = times(dim(Multiplicand), dim(Multiplier)).

:- func times(dim, dim) = dim.

times(Md, Mr) =
    (
        Md = one
    ->
        Mr
    ;
        Mr = one
    ->
        Md
    ;
        Md = unit(Base1)
    ->
        ( Mr = unit(Base2) ->
            ( Base1 = Base2 ->
                power(Md, to_rational(2))
            ;
                product([Md, Mr])
            )
        ; Mr = product(Prod1) ->
            product([Md] ++ Prod1)
        ;
            product([Md] ++ [Mr])
        )
    ;
        Md = power(Base1, Exp1)
    ->
        ( Mr = power(Base2, Exp2) ->
            ( Base1 = Base2 ->
                power(Base1, Exp1 * Exp2)
            ;
                product([Md, Mr])
            )
        ; Mr = product(Prod1) ->
            product([Md] ++ Prod1)
        ; Mr = Base1 ->
            power(Base1, Exp1 + rational.one)
        ;
            product([Md] ++ [Mr])
        )
    ;
        Md = product(Prod1)
    ->
        ( Mr = product(Prod2) ->
            product(Prod1 ++ Prod2)
        ;
            product(Prod1 ++ [Mr])
        )
    ;
        unexpected($file, $pred, "unsupported dimension product")
    ).

Divident / Divisor = Divident * Divisor ** (-1).

Augend + Addend = Sum :-
    DimAu = dim(Augend),
    DimAd = dim(Addend),
    ScaleAu = scale(Augend),
    ScaleAd = scale(Addend),
    ( DimAu = DimAd ->
        Sum = dimmed_value(ScaleAu + ScaleAd, DimAu)
    ;
        Sum = dimmed_value(1.0, sum([ScaleAu, ScaleAd], [DimAu, DimAd]))
    ;
        unexpected($file, $pred, "unsupported dimension sum")
    ).

Minuend - Subtrahend = Minuend + (-1.0 * Subtrahend ).

exp(Value, Exp) = dimmed_value(Scale, dim(Value)) :-
    Scale = scale(Value) * 10.0 ** to_float(Exp).

%------------------------------------------------------------------------------%

metre = unit(length).
m = metre.

astronomical_unit = 149597870700.0 * m.
'AU' = astronomical_unit.

lightyear = 9.4605284e15 * m.
ly = lightyear.

parsec = 3.0857e16 * m.
pc = parsec.

second  = unit(time).
s = second.

kilogram = unit(mass).
kg = kilogram.

kelvin = unit(temperature).
'K' = kelvin.

candela = unit(luminous_intensity).
cd = candela.

mole = unit(amount_of_substance).
mol = mole.

ampere = unit(electric_current).
'A' = ampere.

:- func pi_rad = dimmed_value.

pi_rad = math.pi * rad.

rad = one.
turn = 2.0 * pi_rad.
degree = math.pi / 180.0.
'°' = degree.

%------------------------------------------------------------------------------%
% Pretty print routines to nicely format all SI Units and derived units
%------------------------------------------------------------------------------%

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
        format_list(map_to_univ(Product), str("⋅"))
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
:- mode integer_filter_map_str(in((func(in) = out) is semidet), in) = out.

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

%------------------------------------------------------------------------------%
% Initialisation code for pretty printing
%------------------------------------------------------------------------------%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    get_default_formatter_map(Fmt0, !IO),
    unit_formatter(Fmt0, Fmt),
    set_default_formatter_map(Fmt, !IO).

:- pred unit_formatter(formatter_map::in, formatter_map::out) is det.

unit_formatter(!Fmt) :-
    set_formatter_sv($module, "dim", 0, fmt_dim, !Fmt),
    set_formatter_sv($module, "dimmed_value", 1, fmt_dimmed_value, !Fmt).

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

%------------------------------------------------------------------------------%
:- end_module si_units.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%------------------------------------------------------------------------------%
