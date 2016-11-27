Nonterminals prog list exprs expr funcall arglist nonempty_arglist.
Terminals quote sym int bool left right comma squareleft squareright.
Rootsymbol prog.

prog -> exprs : {prog,'$1'}.
prog -> '$empty' : {prog, {sym, ok}}.

% funcall -> dollar sym left arglist right : cons(remtok('$2'), '$4').
funcall -> sym left arglist right : cons(remtok('$1'), '$3').

nonempty_arglist -> expr : mklist(['$1']).
nonempty_arglist -> expr comma nonempty_arglist : cons('$1', '$3').

arglist -> '$empty' : mklist([]).
arglist -> nonempty_arglist : '$1'.

list -> squareleft squareright : mklist([]).
list -> squareleft exprs squareright : mklist('$2').

exprs -> expr : ['$1'].
exprs -> expr exprs : ['$1'|'$2'].

expr -> sym : remtok('$1').
expr -> bool : remtok('$1').
expr -> int : remtok('$1').
expr -> list : '$1'.
expr -> funcall : '$1'.
expr -> quote expr : mklist([{sym,quote}, '$2']).

Erlang code.
mklist(Ls) -> {list, Ls}.
remtok({T, _L, V}) -> {T, V}.
cons(E, {list, Es}) -> {list, [E|Es]}.
