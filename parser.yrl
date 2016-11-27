Nonterminals
prog exp funcall explist arglist name list letexp bindlist binding math define.

Terminals
sym int bool ';' '=' '(' ')' ',' '[' ']' 'in' 'let' 'end' '*' '+' '/' '-' '>'
'<' '==' 'and' 'or' 'not' '!' '\'' 'fun'.

Rootsymbol prog.

Left 100 '+'.
Left 100 '-'.
Left 200 '*'.
Left 200 '/'.
Left 200 '<'.
Left 200 '>'.
Left 300 'and'.
Left 300 'or'.
Left 400 '=='.
Unary 500 '!'.
Unary 500 'not'.

prog -> explist : {prog, '$1'}.

define ->
    'fun' name '(' arglist ')' '=' exp : mklist([{sym,'define'}, '$2', mklist('$4'), '$7']).
define ->
    'fun' name '(' ')' '=' exp : mklist([{sym,'define'}, '$2', mklist([]), '$6']).

funcall -> name '(' ')' : mklist(['$1']).
funcall -> name '(' arglist ')': mklist(['$1'|'$3']).

explist -> exp ';' : ['$1'].
explist -> exp ';' explist : ['$1'|'$3'].

arglist -> exp : ['$1'].
arglist -> exp ',' arglist : ['$1'|'$3'].

bindlist -> binding : ['$1'].
bindlist -> binding ',' bindlist : ['$1'|'$3'].

letexp -> 'let' bindlist 'in' exp 'end' : mklist([{sym,'let'}, mklist('$2'), '$4']).

binding -> name '=' exp : {list, ['$1', '$3']}.

list -> '[' ']' : {list, []}.
list -> '[' arglist ']' : {list, '$2'}.

name -> sym : remtok('$1').

math -> exp '*' exp : mklist([{sym, 'bintimes'}, '$1', '$3']).
math -> exp '+' exp : mklist([{sym, 'binplus'}, '$1', '$3']).
math -> exp '/' exp : mklist([{sym, 'bindiv'}, '$1', '$3']).
math -> exp '-' exp : mklist([{sym, 'binminus'}, '$1', '$3']).
math -> exp '>' exp : mklist([{sym, 'bingt'}, '$1', '$3']).
math -> exp '<' exp : mklist([{sym, 'binlt'}, '$1', '$3']).
math -> exp '==' exp : mklist([{sym, 'bineq'}, '$1', '$3']).
math -> exp 'or' exp : mklist([{sym, 'or'}, '$1', '$3']).
math -> exp 'and' exp : mklist([{sym, 'and'}, '$1', '$3']).
math -> '!' exp : mklist([{sym, 'not'}, '$2']).
math -> 'not' exp : mklist([{sym, 'not'}, '$2']).

exp -> name : '$1'.
exp -> bool : remtok('$1').
exp -> int : remtok('$1').
exp -> list : mklist([{sym,quote}, '$1']).
exp -> funcall : '$1'.
exp -> letexp : '$1'.
exp -> math : '$1'.
exp -> define : '$1'.
exp -> '\'' exp : mklist([{sym,quote}, '$2']).

Erlang code.
mklist(Ls) -> {list, Ls}.
remtok({T, _L, V}) -> {T, V}.
