Definitions.

D   = [0-9]
L   = [A-Za-z_\-\/\!\?\+\*\=\>\<]
WS  = ([\000-\s]|%.*)
N   = [\n]

Rules.

{L}+   : {token,{sym,TokenLine,list_to_atom(TokenChars)}}.
\#[tf] : {token,{bool,TokenLine,list_to_bool(TokenChars)}}.
{D}+   : {token,{int,TokenLine,list_to_integer(TokenChars)}}.
[(]    : {token,{left,TokenLine}}.
[)]    : {token,{right,TokenLine}}.
[\']   : {token,{quote,TokenLine}}.
{WS}+  : skip_token.
{N}+   : skip_token.

Erlang code.
list_to_bool([$#|"t"]) ->
    true;
list_to_bool([$#|"f"]) ->
    false.
