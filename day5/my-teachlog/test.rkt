#lang teachlog/ugly

rel parent/2.

parent("maekar", "aegon-5").
parent("aegon-5", "aerys-2").
parent("aegon-2", "daenerys").
parent("aegon-2", "drogon").

rel ancestor/2.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :-
  parent(X, Y),
  ancestor(Y, Z).

parent("maekar", "aegon-5")?
parent("maekar", "aegon-4")?

ancestor("maekar", "drogon")?
ancestor(X, "drogon")?

next. next. next. next.

% Data

rel has-type/3.

data one.
data zero.
data plus/2.

data Num.

has-type(Γ, zero, Num).
has-type(Γ, one, Num).

has-type(Γ, plus(X,Y), Num):-
  hastype(Γ, X, Num),
  hastype(Γ, Y, Num).

hastype(Γ, plus(zero, one), T)?