Nonterminals form list elements element.
Terminals quote sym int bool left right.
Rootsymbol form.

form -> elements : {form,'$1'}.
form -> '$empty' : {form, {sym, ok}}.

list -> left right : mklist([]).
list -> left elements right : mklist('$2').

elements -> element : ['$1'].
elements -> element elements : ['$1'|'$2'].

element -> sym : remtok('$1').
element -> bool : remtok('$1').
element -> int : remtok('$1').
element -> list : '$1'.
element -> quote element : mklist([{sym,quote}, '$2']).

Erlang code.
mklist(Ls) -> {list, Ls}.
remtok({T, _L, V}) -> {T, V}.
