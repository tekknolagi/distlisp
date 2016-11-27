Definitions.

D   = [0-9]
L   = [A-Za-z_\.\?]+
WS  = ([\000-\s]|\-\-.*|[\t\n\s\r])

Rules.

let     : {token,{'let',TokenLine}}.
in      : {token,{'in',TokenLine}}.
end     : {token,{'end',TokenLine}}.
fun     : {token,{'fun',TokenLine}}.
(not|or|and|[*-+/><=!]|==) : {token,{list_to_atom(TokenChars),TokenLine}}.
[\(\)\[\]\',\;]     : {token,{list_to_atom(TokenChars),TokenLine}}.
\#[tf]  : {token,{bool,TokenLine,list_to_bool(TokenChars)}}.
{D}+    : {token,{int,TokenLine,list_to_integer(TokenChars)}}.
{L}     : {token,{sym,TokenLine,list_to_atom(TokenChars)}}.
{WS}+   : skip_token.

Erlang code.
list_to_bool([$#|"t"]) -> true;
list_to_bool([$#|"f"]) -> false.
