type stream =
  { mutable line_num: int; mutable chr: char list; chan: in_channel };;

type 'a llist =
  | Nil
  | Cons of 'a * 'a llist;;

type lobject =
  | Fixnum of int
  | Boolean of bool
  | Symbol of string
  | List of lobject llist;;

exception SyntaxError of string;;

(* Read objects. *)

let read_char stm =
    match stm.chr with
      | [] ->
              let c = input_char stm.chan in
              if c = '\n' then let _ = stm.line_num <- stm.line_num + 1 in c
              else c
      | c::rest ->
              let _ = stm.chr <- rest in c

let unread_char stm c =
  stm.chr <- c :: stm.chr;;

let is_white c =
  c = ' ' || c = '\t' || c = '\n';;

let rec eat_whitespace stm =
  let c = read_char stm in
  if is_white c then
    eat_whitespace stm
  else
    unread_char stm c;
    ();;

let rec eat_line stm =
  let c = read_char stm in
  if c != '\n' then
    eat_line stm
  else
    ();;

let rec read_object stm =
  let is_digit c =
    let code = Char.code c in
    code >= Char.code('0') && code <= Char.code('9')
  in
  let is_alpha c =
    let code = Char.code c in
    (code >= Char.code('a') && code <= Char.code('z')) ||
    (code >= Char.code('A') && code <= Char.code('Z'))
  in
  let rec read_fixnum acc =
    let nc = read_char stm in
    if is_digit nc
    then read_fixnum (acc ^ (Char.escaped nc))
    else
      let _ = unread_char stm nc in
      Fixnum(int_of_string acc)
  in
  let rec read_list stm =
    eat_whitespace stm;
    let c = read_char stm in
    if c = ')' then
      Nil
    else
      let _ = unread_char stm c in
      let car = read_object stm in
      let cdr = read_list stm in
      Cons(car, cdr)
  in
  let is_symstartchar c =
    is_alpha c || c = '*' || c = '/' || c = '<' || c = '>'
    || c = '=' || c = '?' || c = '!' || c = '-' || c = '+'
    || c = '@'
  in
  let is_delimiter c =
    is_white c || c = '(' || c = ')' || c = '"' || c = ';'
  in
  let rec read_symbol stm acc =
    let c = read_char stm in
    if is_symstartchar c || is_digit c then
      read_symbol stm (acc ^ (Char.escaped c))
    else
      if is_delimiter c then
        let _ = unread_char stm c in
        Symbol(acc)
      else
        raise (SyntaxError "Symbol not followed by delimiter")
  in
  eat_whitespace stm;
  let c = read_char stm in
  if c = ';' then
    let _ = eat_line stm in read_object stm
  else if c = '#' then
    let nc = read_char stm in
    match nc with
    | 't' -> Boolean(true)
    | 'f' -> Boolean(false)
    | x -> raise (SyntaxError ("Invalid boolean literal " ^ Char.escaped x))
  else if c = '(' then
    List(read_list stm)
  else if is_symstartchar c then
    let _ = unread_char stm c in
    read_symbol stm ""
  else if c = '\'' then
    let obj = read_object stm in
    List(Cons(Symbol("quote"), Cons(obj, Nil)))
  else if (is_digit c) || (c = '~')
  then read_fixnum (Char.escaped (if c='~' then '-' else c))
  else raise (SyntaxError ("Unexpected char " ^ Char.escaped c));;

let rec print_object (obj : lobject) =
  let print_bool obj =
    print_string (match obj with
                  | true -> "true"
                  | false -> "false");
  in
  let rec print_list obj =
    match obj with
    | Cons(car,cdr) ->
        print_object car;
        if cdr=Nil then ()
        else print_string ", ";
        print_list cdr;
    | Nil -> ()
  in
  match obj with
  | Fixnum(value) ->
          print_string "{int, ";
          print_int value;
          print_string "}"
  | Boolean(value) ->
          print_string "{bool, ";
          print_bool value;
          print_string "}"
  | Symbol(value) ->
          print_string "{sym, '";
          print_string value;
          print_string "'}"
  | List(es) ->
          print_string "{list, [";
          print_list es;
          print_string "]}";;

let rec main () =
  let stm = { chr=[]; line_num=1; chan=stdin } in
  (* let _ = print_string "> " in
  let _ = flush stdout in *)
  let obj = read_object stm in
  print_object obj;
  print_string ".";
  print_newline ();
  main ();;

let () =
    try
        main ()
    with End_of_file ->
        ();;
