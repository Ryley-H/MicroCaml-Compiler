open Types
open Utils

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Parsing - Takes a list of tokens and returns an AST representing the MicroCaml expression corresponding to the given 
tokens, along with any tokens left in the token list. *)
let rec parse_expr toks = parse_lets toks

and parse_lets toks = match lookahead toks with
  | Some Tok_Let -> let toks = match_token toks Tok_Let in
      let (toks, is_rec) = match lookahead toks with
        | Some Tok_Rec -> (match_token toks Tok_Rec, true)
        | _ -> (toks, false)
    in let toks, id = parse_id toks in
    let toks = match_token toks Tok_Equal in
    let toks, expr1 = parse_expr toks in
    let toks = match_token toks Tok_In in
    let toks, expr2 = parse_expr toks in (toks, Let (id, is_rec, expr1, expr2))
  | _ -> parse_ifs toks

and parse_ifs toks = match lookahead toks with
  | Some Tok_If -> let toks = match_token toks Tok_If in
    let toks, cond = parse_expr toks in
    let toks = match_token toks Tok_Then in
    let toks, then_expr = parse_expr toks in
    let toks = match_token toks Tok_Else in
    let toks, else_expr = parse_expr toks in (toks, If (cond, then_expr, else_expr))
  | _ -> parse_funs toks

and parse_funs toks = match lookahead toks with
  | Some Tok_Fun -> let toks = match_token toks Tok_Fun in
    let toks, param = parse_id toks in
    let toks = match_token toks Tok_Arrow in
    let toks, body_expr = parse_expr toks in (toks, Fun (param, body_expr))
  | _ -> parse_ors toks

and parse_ors toks = let toks, left_expr = parse_ands toks in match lookahead toks with
  | Some Tok_Or -> let toks = match_token toks Tok_Or in
    let toks, right_expr = parse_ors toks in (toks, Binop (Or, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_ands toks = let toks, left_expr = parse_equalities toks in match lookahead toks with
  | Some Tok_And -> let toks = match_token toks Tok_And in
      let toks, right_expr = parse_ands toks in (toks, Binop (And, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_equalities toks = let toks, left_expr = parse_inequalities toks in match lookahead toks with
  | Some Tok_Equal | Some Tok_NotEqual as op -> let op_tok = match op with
      | Some Tok_Equal -> Equal
      | Some Tok_NotEqual -> NotEqual
      | _ -> raise (InvalidInputException "Invalid Input")
    in let toks = match_token toks (Option.get op) in
    let toks, right_expr = parse_equalities toks in (toks, Binop (op_tok, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_inequalities toks = let toks, left_expr = parse_add_sub toks in match lookahead toks with
  | Some Tok_Less | Some Tok_LessEqual | Some Tok_Greater | Some Tok_GreaterEqual as op -> let op_tok = match op with
      | Some Tok_Less -> Less
      | Some Tok_LessEqual -> LessEqual
      | Some Tok_Greater -> Greater
      | Some Tok_GreaterEqual -> GreaterEqual
      | _ -> raise (InvalidInputException "Invalid Input")
    in let toks = match_token toks (Option.get op) in
    let toks, right_expr = parse_inequalities toks in (toks, Binop (op_tok, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_add_sub toks = let toks, left_expr = parse_mult_div toks in match lookahead toks with
  | Some Tok_Add | Some Tok_Sub as op -> let op_tok = match op with
      | Some Tok_Add -> Add
      | Some Tok_Sub -> Sub
      | _ -> raise (InvalidInputException "Invalid Input")
    in let toks = match_token toks (Option.get op) in
    let toks, right_expr = parse_add_sub toks in (toks, Binop (op_tok, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_mult_div toks = let toks, left_expr = parse_concat toks in match lookahead toks with
  | Some Tok_Mult | Some Tok_Div as op -> let op_tok = match op with
      | Some Tok_Mult -> Mult
      | Some Tok_Div -> Div
      | _ -> raise (InvalidInputException "Invalid Input")
    in let toks = match_token toks (Option.get op) in
    let toks, right_expr = parse_mult_div toks in (toks, Binop (op_tok, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_concat toks = let toks, left_expr = parse_unary toks in match lookahead toks with
  | Some Tok_Concat -> let toks = match_token toks Tok_Concat in
    let toks, right_expr = parse_concat toks in (toks, Binop (Concat, left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_unary toks = match lookahead toks with
  | Some Tok_Not -> let toks = match_token toks Tok_Not in
    let toks, expr = parse_unary toks in (toks, Not expr)
  | _ -> parse_app toks

and parse_app toks = let toks, left_expr = parse_select toks in match lookahead toks with
  | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen -> let toks, right_expr = parse_app toks in (toks, App (left_expr, right_expr))
  | _ -> (toks, left_expr)

and parse_select toks = let toks, primary_expr = parse_primary toks in match lookahead toks with
  | Some Tok_Dot -> let toks = match_token toks Tok_Dot in
    let toks, id = parse_id toks in (toks, Select (Lab id, primary_expr))
  | _ -> (toks, primary_expr)

and parse_primary toks = match lookahead toks with
  | Some (Tok_Int integer) -> (match_token toks (Tok_Int integer), Int integer)
  | Some (Tok_Bool boolean) -> (match_token toks (Tok_Bool boolean), Bool boolean)
  | Some (Tok_String str) -> (match_token toks (Tok_String str), String str)
  | Some (Tok_ID id) -> (match_token toks (Tok_ID id), ID id)
  | Some Tok_LParen -> let toks = match_token toks Tok_LParen in
    let toks, expr = parse_expr toks in
    let toks = match_token toks Tok_RParen in (toks, expr)
  | Some Tok_LCurly -> parse_records toks
  | _ -> raise (InvalidInputException "Invalid Input")

and parse_records toks = let toks = match_token toks Tok_LCurly in match lookahead toks with
  | Some Tok_RCurly -> (match_token toks Tok_RCurly, Record [])
  | _ -> let toks, record_body = parse_rbody toks in
    let toks = match_token toks Tok_RCurly in (toks, Record record_body)

and parse_rbody toks = let toks, id = parse_id toks in
  let toks = match_token toks Tok_Equal in
  let toks, expr = parse_expr toks in match lookahead toks with
  | Some Tok_Semi -> let toks = match_token toks Tok_Semi in
    let toks, rest = parse_rbody toks in (toks, (Lab id, expr) :: rest)
  | _ -> (toks, [(Lab id, expr)])

and parse_id toks = match lookahead toks with
  | Some (Tok_ID id) -> (match_token toks (Tok_ID id), id)
  | _ -> raise (InvalidInputException "Invalid Input")

(* Parsing mutop - Takes a list of tokens and returns an AST representing the MicroCaml expression at the mutop level corresponding to the given tokens, along with any tokens left
in the token list.*)

let rec parse_mutop toks = match lookahead toks with
  | Some Tok_Def -> let toks = match_token toks Tok_Def in
    let toks, id = parse_id toks in
    let toks = match_token toks Tok_Equal in
    let toks, expr = parse_expr toks in
    let toks = match_token toks Tok_DoubleSemi in (toks, Def (id, expr))
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | _ -> let toks, expr = parse_expr toks in let toks = match_token toks Tok_DoubleSemi in (toks, Expr expr)
