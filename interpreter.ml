(* Interpreter for the WHILE language *)
(* Arthur Jacquin - October 2022 *)

(* NOTE: use of Reverse Polish Notation in parsing arithmetic expressions *)
(* NOTE: no support for negative number (do 0 N - instead) *)
(* NOTE: add of the print command *)
(* NOTE: support for tabulations, comments and empty lines *)



(*** DEFINITIONS ***)

(* Identifiers *)
type variable = A | B | C | D | E | F | G
              | H | I | J | K | L | M | N
              | O | P | Q | R | S | T | U
              | V | W | X | Y | Z;;

(* Arithmetic expressions *)
type arithm = N of int
            | V of variable
            | Plus of arithm * arithm
            | Minus of arithm * arithm
            | Mult of arithm * arithm;;

(* Boolean expressions *)
type boolean = True
             | False
             | Equal of arithm * arithm
             | Lower of arithm * arithm
             | Not of boolean
             | Or of boolean * boolean
             | And of boolean * boolean;;

(* Commands *)
type command = Skip
             | Affect of variable * arithm
             | Concat of command * command
             | If of boolean * command * command
             | While of boolean * command
             | Print of arithm;;

(* Memory *)
type sigma = (variable -> int);;

(* Errors *)
exception ParsingError;;
exception InvalidNumberOfArguments;;



(*** INTERPRETATION ***)

let rec int_arithm (a: arithm) (m: sigma) : int =
    match a with
    | N n -> n
    | V v -> m v
    | Plus (x, y) -> (int_arithm x m) + (int_arithm y m)
    | Minus (x, y) -> (int_arithm x m) - (int_arithm y m)
    | Mult (x, y) -> (int_arithm x m) * (int_arithm y m);;

let rec int_boolean (b: boolean) (m: sigma) : bool =
    match b with
    | True -> true
    | False -> false
    | Equal (x, y) -> (int_arithm x m) = (int_arithm y m)
    | Lower (x, y) -> (int_arithm x m) <= (int_arithm y m)
    | Not x -> if (int_boolean x m = true) then false else true
    | Or (x, y) -> (int_boolean x m) || (int_boolean y m)
    | And (x, y) -> (int_boolean x m) && (int_boolean y m);;

let rec int_command (c: command) (m: sigma) : sigma =
    match c with
    | Skip -> m
    | Affect (v, a) -> (fun w -> if v = w then (int_arithm a m) else (m w))
    | Concat (x, y) -> int_command y (int_command x m)
    | If (b, x, y) -> if (int_boolean b m = true) then (int_command x m) else (int_command y m)
    | While (b, x) -> if (int_boolean b m = true) then int_command (While (b, x)) (int_command x m) else m
    | Print a -> (print_int (int_arithm a m); print_string "\n"; m);;



(*** PARSING ***)

let parse_variable (s: string) : variable =
    match s with
    | "A" -> A
	| "B" -> B
	| "C" -> C
    | "D" -> D
    | "E" -> E
    | "F" -> F
    | "G" -> G
    | "H" -> H
    | "I" -> I
    | "J" -> J
    | "K" -> K
    | "L" -> L
    | "M" -> M
    | "N" -> N
    | "O" -> O
    | "P" -> P
    | "Q" -> Q
    | "R" -> R
    | "S" -> S
    | "T" -> T
    | "U" -> U
    | "V" -> V
    | "W" -> W
	| "X" -> X
	| "Y" -> Y
    | "Z" -> Z
    | _ -> raise ParsingError;;

let parse_arithm (s: string) : arithm = (* Reverse Polish Notation *)
    let pile : arithm list ref = ref []
    and n : int = String.length s
    and i : int ref = ref 0
    and number_start : int ref = ref 0
    and reading_integer : bool ref = ref false
    in begin
    while !i < n do
        let new_pile, increment =
            match int_of_char (s.[!i]), !pile, !reading_integer with
            | (* space *) 32, p, true ->
                begin
                    reading_integer := false;
                    (N (int_of_string (String.sub s !number_start (!i - !number_start))))::(!pile), 1
                end
            | (* 0-9 *) c, p, true when (48 <= c) && (c < 58) -> p, 1
            | (* 0-9 *) c, p, false when (48 <= c) && (c < 58) ->
                begin
                    reading_integer := true;
                    number_start := !i;
                    p, 1
                end
            | (* A-Z *) c, p, false when (65 <= c) && (c <= 90) ->
                    (V (parse_variable (Char.escaped s.[!i])))::p, 2
            | (* + *) 43, a::b::q, false -> (Plus (b, a))::q, 2
            | (* - *) 45, a::b::q, false -> (Minus (b, a))::q, 2
            | (* * *) 42, a::b::q, false -> (Mult (b, a))::q, 2
            | _ -> raise ParsingError
        in (pile := new_pile; i := !i + increment)
    done;
    if !reading_integer then
        pile := (N (int_of_string (String.sub s !number_start (n - !number_start))))::(!pile);
    match !pile with
    | res::[] -> res
    | _ -> raise ParsingError;
    end;;

let find_w_X (s: string) (w: string) : (bool * string) =
    let n = String.length s
    and nw = String.length w in
        if nw <= n then (String.sub s 0 nw = w, String.sub s nw (n - nw))
        else (false, "");;

let find_X_w (s: string) (w: string) : (bool * string) =
    let n = String.length s
    and nw = String.length w in
        if nw <= n then (String.sub s (n - nw) nw = w, String.sub s 0 (n - nw))
        else (false, "");;

let find_w_X_v (s: string) (w: string) (v: string) : (bool * string) =
    let b1, x = find_w_X s w in
    let b2, y = find_X_w x v in
        (b1 && b2, y);;

let find_X_w_Y (s: string) (w: string) : (bool * string * string) =
    let n = String.length s
    and nw = String.length w
    and i = ref 0
    in begin
        while (!i <= n - nw) && not(String.sub s !i nw = w) do
            i := !i + 1;
        done;
        if !i > n - nw then (false, "", "")
        else (true, String.sub s 0 !i, String.sub s (!i + nw) (n - !i - nw))
    end;;

let find_X_w_Y_v (s: string) (w: string) (v: string) : (bool * string * string) =
    let b1, x, y_v = find_X_w_Y s w in
    let b2, y = find_X_w y_v v in
        (b1 && b2, x, y);;

let find_w_X_v_Y_u (s: string) (w: string) (v: string) (u: string) : (bool * string * string) =
    let b1, x = find_w_X s w in
    let b2, y, z = find_X_w_Y_v x v u in
        (b1 && b2, y, z);;

let strip (s: string) : string =
    let n = String.length s
    and i = ref 0 in
        while !i < n && s.[!i] = ' ' do
            i := !i + 1
        done;
        String.sub s !i (n - !i);;

let rec parse_boolean (s: string) : boolean =
    if s = "TRUE" then True
    else if s = "FALSE" then False
    else let b, x, y = find_w_X_v_Y_u s "(" ") AND (" ")" in if b then And (parse_boolean x, parse_boolean y)
    else let b, x, y = find_w_X_v_Y_u s "(" ") OR ("  ")" in if b then Or (parse_boolean x, parse_boolean y)
    else let b, x    = find_w_X_v     s "NOT ("       ")" in if b then Not (parse_boolean x)
    else let b, x, y = find_X_w_Y     s     " == "        in if b then Equal (parse_arithm x, parse_arithm y)
    else let b, x, y = find_X_w_Y     s     " <= "        in if b then Lower (parse_arithm x, parse_arithm y)
    else raise ParsingError;;

let rec parse_program (ic: in_channel) : command =
    let res : command ref = ref Skip
    and continue : bool ref = ref true
    and parse_instruction (s: string) : command =
        let b, x, y   = find_X_w_Y s " := " in if b then
            Affect (parse_variable x, parse_arithm y)
        else let b, x = find_w_X_v s "if " " then {" in if b then
            let then_block = parse_program ic in
            let else_block = parse_program ic in
                If (parse_boolean x, then_block, else_block)
        else let b, x = find_w_X_v s "while " " do {" in if b then
            While (parse_boolean x, parse_program ic)
        else let b, x = find_w_X   s "print " in if b then
            Print (parse_arithm x)
        else raise ParsingError
    in begin
        while !continue do
            try let line : string = strip (input_line ic) in
                if line = "}" || line = "} else {" then continue := false
                else if line = "" || line.[0] = '#' then ()
                else res := Concat (!res, parse_instruction line)
            with error -> match error with
                | End_of_file -> (close_in ic; continue := false)
                | ParsingError -> (close_in ic; raise ParsingError)
                | e -> (close_in_noerr ic; raise e)
        done;
        !res;
    end;;

let parse (file_name: string) : command =
    let ic : in_channel = open_in file_name in
        parse_program ic;;



(*** MAIN FUNCTION ***)

let main : sigma =
    if (Array.length Sys.argv) != 2 then raise InvalidNumberOfArguments
    else
        let initial_memory : sigma = (fun v -> 0)
        and program : command = parse (Sys.argv.(1))
        in int_command program initial_memory;;
