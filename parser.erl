-module(parser).
-export([read_object/1, read_object/0]).

-record(stream, {line_num=0, buffer=[]}).

getchar(#stream{line_num=LineNum, buffer=[]}) ->
    [C|_] = io:get_chars("", 1),
    {C, #stream{line_num=LineNum, buffer=[]}};

getchar(#stream{line_num=LineNum, buffer=[H|T]}) ->
    {H, #stream{line_num=LineNum, buffer=T}}.


unread(#stream{line_num=LineNum, buffer=B}, C) ->
    #stream{line_num=LineNum, buffer=[C|B]}.


is_white($\ ) -> true;
is_white($\t) -> true;
is_white($\n) -> true;
is_white(_)   -> false.


eat_whitespace(Stream) ->
    {C, Stream1} = getchar(Stream),
    case is_white(C) of
        true  -> eat_whitespace(Stream);
        false -> unread(Stream1, C)
    end.


is_digit(C) ->
    (C >= $0) and (C =< $9).


is_symstartchar($() -> true;
is_symstartchar($)) -> true;
is_symstartchar($") -> true;
is_symstartchar($;) -> true;
is_symstartchar(C)  -> is_white(C).


%is_alpha(C) ->
%    ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)).

read_fixnum(Stream, Acc) ->
    {C, Stream1} = getchar(Stream),
    case is_digit(C) of
        true  -> read_fixnum(Stream1, Acc ++ [C]);
        false ->
            {Num, []} = string:to_integer(Acc),
            {{int, Num}, unread(Stream1, C)}
    end.

read_symbol(Stream, Acc) ->
    {C, Stream1} = getchar(Stream),
    case classify_char(C) of
        begins_sym ->
            read_symbol(Stream1, Acc ++ [C]);
        delimiter ->
            {{sym, Acc}, unread(Stream1, C)};
        other ->
            erlang:error({bad_symbol, non_delimiter, C})
    end.

read_list(Stream) ->
    Stream1 = eat_whitespace(Stream),
    {C, Stream2} = getchar(Stream1),
    if
        C == $) -> {[], Stream2};
        true ->
            Stream3 = unread(Stream2, C),
            {Car, Stream4} = read_object(Stream3),
            {Cdr, Stream5} = read_object(Stream4),
            {[Car|Cdr], Stream5}
    end.

read_object() ->
    read_object(newstream()).

read_object(Stream) ->
    Stream1 = eat_whitespace(Stream),
    {C, Stream2} = getchar(Stream1),
    io:format("looking at ~p~n", [[C]]),
    case classify_char(C) of
        begins_bool ->
            {NC, Stream3} = getchar(Stream2),
            {case NC of
                $t -> {bool, true};
                $f -> {bool, false};
                true -> erlang:error({bad_boolean, NC})
            end, Stream3};
        begins_int ->
            read_fixnum(Stream2, [if C == $~ -> $-; true -> C end]);
        begins_sym ->
            read_symbol(Stream2, [C]);
        begins_list ->
            {Ls, Stream3} = read_list(Stream2),
            {{list, Ls}, Stream3};
        delimiter ->
            erlang:error({unexpected_delimiter, [C]});
        other ->
            erlang:error({bad_pattern, C})
    end.


newstream() ->
    #stream{line_num=0, buffer=[]}.


classify_char($#) -> begins_bool;
classify_char($~) -> begins_int;
classify_char(C) when (C >= $0) and (C =< $9) -> begins_int;
classify_char(C) when (C >= $a) and (C =< $z) -> begins_sym;
classify_char(C) when (C >= $A) and (C =< $Z) -> begins_sym;
classify_char($*) -> begins_sym;
classify_char($/) -> begins_sym;
classify_char($<) -> begins_sym;
classify_char($>) -> begins_sym;
classify_char($=) -> begins_sym;
classify_char($?) -> begins_sym;
classify_char($!) -> begins_sym;
classify_char($-) -> begins_sym;
classify_char($+) -> begins_sym;
classify_char($@) -> begins_sym;
classify_char($') -> begins_sym;
classify_char($() -> begins_list;
classify_char(C) when (C == $\ ) or (C == $\t) or (C == $\n) -> delimiter;
classify_char(C) when (C == $)) or (C == $") or (C == $;) ->
    delimiter;
classify_char(_) -> other.
