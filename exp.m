%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: exp.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon Jan 12 11:56:33 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module si_units.exp.

:- interface.

:- use_module rational.

:- import_module generic_math.

%----------------------------------------------------------------------------%

:- type exp
    --->    one
    ;       two
    ;       three
    ;       i(int)
    ;       r(rational.rational).

:- inst one     ---> one.
:- inst two     ---> two.
:- inst three   ---> three.
:- inst i       ---> i(ground).
:- inst i1      ---> i(bound(1)).
:- inst i2      ---> i(bound(2)).
:- inst i3      ---> i(bound(3)).
:- inst r       ---> r(ground).

:- instance scalar_generic_math(exp).
:- instance generic_math(exp).

:- func to_exp(T) = exp <= scalar_generic_math(T).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- use_module math.

:- import_module exception.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

:- instance scalar_generic_math(exp) where [
    func(times_float/2) is exp_times_float,
    func(to_int/1) is exp_to_int,
    func(to_integer/1) is exp_to_integer,
    func(to_float/1) is exp_to_float,
    func(to_rational/1) is exp_to_rational
].

:- func exp_times_float
    `with_type` bin_op_func(exp, float) `with_inst` bin_op_func_uo.

exp_times_float(Exp, Float) = _ :- sorry($file, $pred).

:- func exp_to_int(exp) = int is semidet.

exp_to_int(T) = _ :- sorry($file, $pred).

:- func exp_to_integer(exp) = bigint is semidet.

exp_to_integer(T) = _ :- sorry($file, $pred).

:- func exp_to_float(exp) = float.

exp_to_float(one)    = 1.0.
exp_to_float(two)    = 2.0.
exp_to_float(three)  = 3.0.
exp_to_float(i(I))   = to_float(I).
exp_to_float(r(R))   = to_float(R).

:- func exp_to_rational(exp) = rational.rational.

exp_to_rational(one)    = rational.rational(1).
exp_to_rational(two)    = rational.rational(2).
exp_to_rational(three)  = rational.rational(3).
exp_to_rational(i(I))   = rational.rational(I).
exp_to_rational(r(R))   = R.

%----------------------------------------------------------------------------%

:- instance generic_math(exp) where [
    func(abs/1) is exp_abs,
    func(min/2) is exp_min,
    func(max/2) is exp_max,
    func(times/2) is exp_times,
    func(divide/2) is exp_divide,
    func(pow/2) is exp_pow,
    func(add/2) is exp_add,
    func(substract/2) is exp_substract
].

:- func exp_abs `with_type` unary_op_func(exp).

exp_abs(T) = _ :- sorry($file, $pred).

:- func exp_min `with_type` bin_op_func(exp).

exp_min(A, B) = _ :- sorry($file, $pred).

:- func exp_max `with_type` bin_op_func(exp).

exp_max(A, B) = _ :- sorry($file, $pred).

:- func exp_times `with_type` bin_op_func(exp).

exp_times(A, B) =
    (if Product = exp_times_norm(norm(A), norm(B)) then
        Product
    else
        unexpected($file, $pred, "should not happen if `norm/1' works")
    ).

:- func exp_times_norm(exp, exp) = exp.
:- mode exp_times_norm(in, in) = out is semidet.
:- mode exp_times_norm(in(i), in(i)) = out(i) is det.
:- mode exp_times_norm(in(i), in(r)) = out(r) is det.
:- mode exp_times_norm(in(r), in(i)) = out(r) is det.

exp_times_norm(i(A), i(B)) = i(A * B).
exp_times_norm(i(A), r(B)) = r(to_rational(A) * B).
exp_times_norm(r(A), i(B)) = r(A * to_rational(B)).

:- func exp_divide `with_type` bin_op_func(exp).

exp_divide(Dividend, Divisor) = _ :- sorry($file, $pred).

:- func exp_pow `with_type` bin_op_func(exp).

exp_pow(Base, Exp) = _ :- sorry($file, $pred).

:- func exp_add `with_type` bin_op_func(exp).

exp_add(Augend0, Addend0) = Sum :-
    Augend = norm(Augend0),
    Addend = norm(Addend0),
    Sum =
        ( if
            Augend = i(AugendI),
            Addend = i(AddendI)
        then
            i(AugendI + AddendI)
        else
            sorry($file, $pred)
        ).

:- func exp_substract `with_type` bin_op_func(exp).

exp_substract(Minuend, Subtrahend) = _ :- sorry($file, $pred).

%----------------------------------------------------------------------------%

:- func norm(exp) = exp.
:- mode norm(in) = out is det.
:- mode norm(in(one)) = out(i1) is det.
:- mode norm(in(two)) = out(i2) is det.
:- mode norm(in(three)) = out(i3) is det.
:- mode norm(in(i)) = out(i) is det.
:- mode norm(in(r)) = out(r) is det.

norm(one)   = i(1).
norm(two)   = i(2).
norm(three) = i(3).
norm(i(I))  = i(I).
norm(r(R))  = r(R).

:- func denorm(exp) = exp.
:- mode denorm(in) = out is semidet.
:- mode denorm(in(i1)) = out(one) is det.
:- mode denorm(in(i2)) = out(two) is det.
:- mode denorm(in(i3)) = out(three) is det.

denorm(i(1))   = one.
denorm(i(2))   = two.
denorm(i(3))   = three.

to_exp(V) =
    (if I = to_int(V) then
        i(I)
    else
        r(to_rational(V))
    ).

%----------------------------------------------------------------------------%
:- end_module si_units.exp.
%----------------------------------------------------------------------------%
