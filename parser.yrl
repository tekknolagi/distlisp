Nonterminals
prog exp funcall explist arglist name list letexp bindlist binding opexp define
letkind lambda lisplist opfn oper unaryoper quotable.

Terminals
sym int bool ';' '=' '(' ')' ',' '[' ']' 'in' 'let' 'let*' 'end' '*' '+' '/'
'-' '>' '<' '<=' '>=' '==' 'and' 'or' 'not' '!' '\'' 'val' 'fun' 'if' 'then'
'else' 'fn' 'op'.

Rootsymbol prog.

% TODO: FIX OPERATOR PRECEDENCE
% AS IN: 1 < 5 or 1 == 5
Unary 100 '!'.
Unary 100 'not'.
Left 200 '+'.
Left 200 '-'.
Left 300 '*'.
Left 300 '/'.
Left 400 '<'.
Left 400 '>'.
Left 400 '>='.
Left 400 '<='.
Left 500 '=='.
Left 600 'and'.
Left 600 'or'.

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

letkind -> 'let' : 'letx'.
letkind -> 'let*' : 'letx*'.

letexp ->
    letkind bindlist 'in' exp 'end' : mklist([{sym,'$1'}, mklist('$2'), '$4']).

binding -> 'val' name '=' exp : {list, ['$2', '$4']}.
binding ->
    'fun' name '(' arglist ')' '=' exp :
        {list, ['$2', {list, [{sym,lambda}, mklist('$4'), '$7']}]}.

list -> '[' ']' : {list, []}.
list -> '[' lisplist ']' : {list, '$2'}.

lisplist -> exp : ['$1'].
lisplist -> exp lisplist : ['$1'|'$2'].

name -> sym : remtok('$1').

oper -> '*' : {sym,bintimes}.
oper -> '+' : {sym,binplus}.
oper -> '/' : {sym,bindiv}.
oper -> '-' : {sym,binminus}.
oper -> '>' : {sym,bingt}.
oper -> '<' : {sym,binlt}.
oper -> '<=' : {sym,binlte}.
oper -> '>=' : {sym,bingte}.
oper -> '==' : {sym,bineq}.
oper -> 'or' : {sym,'or'}.
oper -> 'and' : {sym,'and'}.

unaryoper -> '!' : {sym,'not'}.
unaryoper -> 'not' : {sym,'not'}.

quotable -> name : '$1'.
quotable -> bool : '$1'.
quotable -> int : '$1'.
quotable -> list : '$1'.

opexp -> exp oper exp : mklist(['$2', '$1', '$3']).
opexp -> unaryoper exp : mklist(['$1', '$2']).
opexp -> '\'' quotable : mklist([{sym,quote},'$2']).

lambda ->
    'fn' '(' arglist ')' '=' exp : mklist([{sym, 'lambda'}, mklist('$3'), '$6']).

opfn -> 'op' oper : '$2'.

exp -> name : '$1'.
exp -> bool : remtok('$1').
exp -> int : remtok('$1').
exp -> list : '$1'.
exp -> funcall : '$1'.
exp -> letexp : '$1'.
exp -> opexp : '$1'.
exp -> define : '$1'.
exp -> opfn : '$1'.
exp -> 'val' name '=' exp : mklist([{sym,'valx'},'$2','$4']).
exp -> 'if' exp 'then' exp 'else' exp : mklist([{sym,'ifx'},'$2','$4','$6']).
exp -> '(' exp ')' : '$2'.
exp -> lambda : '$1'.

Erlang code.
mklist(Ls) -> {list, Ls}.
remtok({T, _L, V}) -> {T, V}.
