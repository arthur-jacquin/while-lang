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

(* Check on argument number *)
if (Array.length Sys.argv) != 2 then raise InvalidNumberOfArguments;;

(* First read, to convert the script to a string *)

let strip (s: string) : string =
    let n = String.length s
    and i = ref 0 in
    begin
        while !i < n && s.[!i] = ' ' do
            i := !i + 1
        done;
        String.sub s !i (n - !i);
    end;;

let string_of_file (file_name: string) : string =
    let ic : in_channel = open_in file_name in
    let rec aux (acc: string) : string =
        try let line : string = strip (input_line ic) in
            if line = "" || line.[0] = '#' then aux acc
            else aux (acc ^ line)
        with End_of_file -> (close_in ic; acc)
    in aux "";;

let s : string = string_of_file (Sys.argv.(1));;

(* Iterator on the newly built string (always going forward) *)

let n : int = String.length s;;
let i : int ref = ref 0;;

let collect () : char =
    if !i = n then raise End_of_file
    else let c = s.[!i] in (i := !i + 1; c);;

(* Various tools *)

let check (w: string) : unit =
    (* Check if w is the next sequence to be read *)
    (* NOTE: consume the string *)
    let nw : int = String.length w in
    let rec aux (k: int) : unit =
        if k < nw then
            try if collect() != w.[k] then raise ParsingError
                else aux (k+1)
            with End_of_file -> raise ParsingError
    in aux 0;;

let is_upper_letter (c: char) : bool =
    let x : int = int_of_char c in (65 <= x) && (x < 91);;

let is_digit (c: char) : bool =
    let x : int = int_of_char c in (48 <= x) && (x < 58);;

let digit_of_char (c: char) : int =
    int_of_char c - 48;;

let var_of_char (c: char) : variable =
    match c with
    | 'A' -> A
	| 'B' -> B
	| 'C' -> C
    | 'D' -> D
    | 'E' -> E
    | 'F' -> F
    | 'G' -> G
    | 'H' -> H
    | 'I' -> I
    | 'J' -> J
    | 'K' -> K
    | 'L' -> L
    | 'M' -> M
    | 'N' -> N
    | 'O' -> O
    | 'P' -> P
    | 'Q' -> Q
    | 'R' -> R
    | 'S' -> S
    | 'T' -> T
    | 'U' -> U
    | 'V' -> V
    | 'W' -> W
	| 'X' -> X
	| 'Y' -> Y
    | 'Z' -> Z
    | _ -> raise ParsingError;;

(* Second read, to convert the string to a command object *)

let parse_variable () : variable =
    match collect() with
    | c when is_upper_letter c -> var_of_char c
    | _ -> raise ParsingError;;

let parse_arithm () : arithm =
    (* NOTE: use of the Reverse Polish Notation *)
    let rec aux (recording_integer: bool) (acc: int) (spacing: bool) (stack: arithm list) : arithm =
        (* recording_integer: wether an integer is being read *)
        (* acc: the integer being read, or 0 if there is none *)
        (* spacing: wether a space (or end) is expected next *)
        (* stack: the RPN stack *)
        match collect(), stack, recording_integer, spacing with
        | ']', [],      true,  false -> N acc
        | ']', res::[], false, true  -> res
        | c,   _,       _,     true when c != ' ' -> raise ParsingError
        | ' ', p,       true,  false -> aux false 0 false ((N acc)::p)
        | ' ', p,       false, true  -> aux false 0 false p
        | c,   p,       _,     false   when is_digit c ->
                                        aux true (10*acc + digit_of_char c) false p
        | c,   p,       false, false when is_upper_letter c ->
                                        aux false 0 true ((V (var_of_char c))::p)
        | '+', a::b::q, false, false -> aux false 0 true ((Plus  (b, a))::q)
        | '-', a::b::q, false, false -> aux false 0 true ((Minus (b, a))::q)
        | '*', a::b::q, false, false -> aux false 0 true ((Mult  (b, a))::q)
        | _ -> raise ParsingError
    in aux false 0 false [];;

let rec parse_boolean () : boolean =
    match collect() with
    | 'T' -> (check "RUE"; True) (* True *) 
    | 'F' -> (check "ALSE"; False) (* False *)
    | '(' ->
        begin
            let b1 : boolean = parse_boolean() in
            begin
                check ") ";
                match collect() with
                | 'O' -> (* Or *)
                    begin
                        check "R (";
                        let b2 : boolean = parse_boolean() in
                        begin
                            check ")";
                            Or (b1, b2);
                        end;
                    end
                | 'A' -> (* And *) 
                    begin
                        check "ND (";
                        let b2 : boolean = parse_boolean() in
                        begin
                            check ")";
                            And (b1, b2);
                        end;
                    end
                | _ -> raise ParsingError
            end
        end
    | 'N' -> (* Not *)
        begin
            check "OT (";
            let b : boolean = parse_boolean() in
            begin
                check ")";
                Not b;
            end;
        end
    | '[' ->
        begin
            let a1 : arithm = parse_arithm() in
            begin
                check " ";
                match collect() with
                | '=' -> (* Equal *)
                    begin
                        check "= [";
                        let a2 : arithm = parse_arithm() in
                            Equal (a1, a2);
                    end
                | '<' -> (* Lower *) 
                    begin
                        check "= [";
                        let a2 : arithm = parse_arithm() in
                            Lower (a1, a2);
                    end
                | _ -> raise ParsingError
            end
        end
    | _ -> raise ParsingError;;

let rec parse_block () : command =
    let parse_instruction (c: char) : command =
        match c with
        | _ when is_upper_letter c -> (* Affect *)
            begin
                check " := [";
                let a = parse_arithm()
                and v = var_of_char c in
                begin
                    check ";";
                    Affect (v, a);
                end;
            end
        | 'i' -> (* If *)
            begin
                check "f "; 
                let condition = parse_boolean() in
                begin
                    check " then {";
                    let then_block = parse_block() in
                    begin
                        check " else {";
                        let else_block = parse_block() in
                        begin
                            check ";";
                            If (condition, then_block, else_block);
                        end;
                    end;
                end;
            end
        | 'w' -> (* While *) 
            begin
                check "hile "; 
                let condition : boolean = parse_boolean() in
                begin
                    check " do {";
                    let while_block = parse_block() in
                    begin
                        check ";";
                        While (condition, while_block);
                    end;
                end;
            end
        | 'p' -> (* Print *)
            begin
                check "rint ["; 
                let a : arithm = parse_arithm() in
                begin
                    check ";";
                    Print a;
                end;
            end
        | _ -> raise ParsingError
    in let rec aux (parsed: command) : command =
        try match collect() with
            | '}' -> parsed (* End of bloc *)
            | c -> aux (Concat (parsed, parse_instruction c))
        with
            | End_of_file -> parsed
            | ParsingError -> raise ParsingError
            | e -> raise e
    in aux Skip;;

let program : command = parse_block ();;



(*** MAIN ***)

let initial_memory : sigma = (fun v -> 0);;
let res : sigma = int_command program initial_memory;;
