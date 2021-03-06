Definitions.

D   = [0-9]
L   = [a-zA-Z_][A-Za-z_\.\?]*
WS  = ([\000-\s]|\-\-.*|[\t\n\s\r])

Rules.

(op|if|not|or|and|let|let\*|in|end|val|fun|fn|else|then|;;) :
    {token,{list_to_atom(TokenChars),TokenLine}}.
(>=|<=|[*\-+/><=!]|==) : {token,{list_to_atom(TokenChars),TokenLine}}.
[\(\)\[\]\',\;] : {token,{list_to_atom(TokenChars),TokenLine}}.
\#[tf]  : {token,{bool,TokenLine,list_to_bool(TokenChars)}}.
{D}+    : {token,{int,TokenLine,list_to_integer(TokenChars)}}.
{L}     : {token,{sym,TokenLine,list_to_atom(TokenChars)}}.
\.\.\.  : {token,{sym,TokenLine,list_to_atom(TokenChars)}}.
{WS}+   : skip_token.

Erlang code.
list_to_bool([$#|"t"]) -> true;
list_to_bool([$#|"f"]) -> false.
