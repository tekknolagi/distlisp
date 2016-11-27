Definitions.

D   = [0-9]
L   = [A-Za-z_\-\/\!\?\+\*\=\>\<\.][A-Za-z_\-\/\!\?\+\*\=\>\<\.0-9]*
WS  = ([\000-\s]|;.*|[\t\n\s\r])

Rules.

{D}+    : {token,{int,TokenLine,list_to_integer(TokenChars)}}.
{L}     : {token,{sym,TokenLine,list_to_atom(TokenChars)}}.
\#[tf]  : {token,{bool,TokenLine,list_to_bool(TokenChars)}}.
[(]     : {token,{left,TokenLine}}.
[)]     : {token,{right,TokenLine}}.
[\[]    : {token,{squareleft,TokenLine}}.
[\]]    : {token,{squareright,TokenLine}}.
[\']    : {token,{quote,TokenLine}}.
[,]     : {token,{comma,TokenLine}}.
{WS}+   : skip_token.

Erlang code.
list_to_bool([$#|"t"]) ->
    true;
list_to_bool([$#|"f"]) ->
    false.
