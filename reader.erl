-module(reader).
-export([repl/2, repl/3, read_program/2]).

read_program(string, Data) ->
    {ok, T, _} = scanner:string(Data),
    {ok, {prog, Prog}} = parser:parse(T),
    Prog;

read_program(file, FileName) ->
    {ok, Data} = file:read_file(FileName),
    read_program(string, binary:bin_to_list(Data)).

getchar(eof) -> throw(eof);
getchar([C]) -> C.
getchar() -> getchar(io:get_chars("", 1)).

read_term() ->
    case getchar() of
        $; -> ";";
        C -> [C|read_term()]
    end.

eval_input(Env) ->
    Text = read_term(),
    % Strip all leading whitespace.
    RStripped = re:replace(Text, "^\\s+", "", [global,{return,list}]),
    case RStripped of
        % Don't bother printing a prompt if there's nothing to do.
        [] -> throw(nothing);
        _ -> Prog = read_program(string, RStripped),
             eval:evalexp(Prog, Env)
    end.

-define(NEXT, reader:repl(Num+1, Env)).
-define(NEXTWITHIT, reader:repl(Num+1, eval:bind(it, Val, NewEnv))).

goodbye() -> io:format("Thank you for trying DLisp.~n"),
             halt(0).

repl(Num, Env) -> repl(Num, Env, true).
repl(Num, Env, ShouldPrint) ->
    case ShouldPrint of
        true -> io:format("~p> ", [Num]);
        false -> ok
    end,
    try eval_input(Env) of
        {{sym, quit}, _} ->
            goodbye();
        {Val, NewEnv} when Val =:= {sym, ok} ->
            ?NEXTWITHIT;
        {Val, NewEnv} ->
            eval:printexp(Val),
            io:format("~n"),
            %io:format(" : ~p~n", [eval:type(Val)]),
            ?NEXTWITHIT
    catch
        throw:code_reload ->
            io:format("WARNING: Code reloaded~n"),
            reader:repl(Num+1, eval:bind(it, {sym, ok}, Env));
        throw:nothing ->
            reader:repl(Num, Env, false);
        throw:eof ->
            goodbye();
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
            ?NEXT
%            ;
%        error:E ->
%            io:format("UNKNOWN ERROR: ~p~n", [E]),
%            ?NEXT
    end.
