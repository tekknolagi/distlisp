-module(reader).
-export([repl/2, read_program/2]).

read_program(string, Data) ->
    {ok, T, _} = scanner:string(Data),
    {ok, {form, Prog}} = parser:parse(T),
    Prog;

read_program(file, FileName) ->
    {ok, Data} = file:read_file(FileName),
    read_program(string, binary:bin_to_list(Data)).

getchar() -> [C] = io:get_chars("", 1),
             C.

read_term(Buf, BraceStack) ->
    C = getchar(),
    case C of
        C when (C == $\n) or (C == $\r) ->
            case BraceStack of
                [] -> lists:reverse(Buf);
                _ -> io:format("?? "),
                     read_term(Buf, BraceStack)
            end;
        $( ->
            read_term([C|Buf], [C|BraceStack]);
        $) ->
            case BraceStack of
                [$(] -> lists:reverse([C|Buf]);
                [$(|RestBraces] -> read_term([C|Buf], RestBraces);
                _ -> erlang:error({syntax_error, mismatched_braces})
            end;
        _ ->
            read_term([C|Buf], BraceStack)
    end.

eval_input(Env) ->
    Text = read_term([], []),
    % Strip all leading whitespace.
    RStripped = re:replace(Text, "^\\s+", "", [global,{return,list}]),
    case RStripped of
        % Don't bother printing a prompt if there's nothing to do.
        [] -> throw(nothing);
        _ -> Prog = read_program(string, RStripped),
             eval:evalexp(Prog, Env)
    end.

-define(NEXT, repl(Num+1, Env)).

repl(Num, Env) -> repl(Num, Env, true).
repl(Num, Env, ShouldPrint) ->
    case ShouldPrint of
        true -> io:format("~p> ", [Num]);
        false -> ok
    end,
    try eval_input(Env) of
        {Val, NewEnv} ->  if Val == {sym, ok} ->
                                 repl(Num+1, eval:bind(it, Val, NewEnv));
                             Val == {sym, quit} ->
                                 io:format("Thank you for trying DLisp.~n");
                             true ->
                                 eval:printexp(Val),
                                 io:format("~n"),
                                 repl(Num+1, eval:bind(it, Val, NewEnv))
                          end
    catch
        throw:code_reload ->
            io:format("WARNING: Code reloaded~n"),
            repl(Num+1, eval:bind(it, {sym, ok}, Env));
        throw:nothing ->
            repl(Num, Env, false);
        error:{unbound_variable,V} ->
            io:format("ERROR: Unbound variable ~p~n", [V]),
            ?NEXT;
        error:{badmatch, G} ->
            io:format("ERROR: Type mismatch: ~p~n", [G]),
            ?NEXT;
        error:{syntax_error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            ?NEXT;
        error:{tuplezip_mismatch, _, _} ->
            io:format("ERROR: Wrong number of arguments~n"),
            ?NEXT;
        error:E ->
            io:format("UNKNOWN ERROR: ~p~n", [E]),
            ?NEXT
    end.
