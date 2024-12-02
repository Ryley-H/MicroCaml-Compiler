open Types
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let length = String.length input in
  let rec helper index =
    if index >= length then []
    else if string_match (regexp "(\\(-[0-9]+\\))") input index then
      let value = matched_string input in
      let int_value = String.sub value 1 (String.length value - 2) in
      Tok_Int (int_of_string int_value) :: helper (index + String.length value)
    else if string_match (regexp "[0-9]+") input index then
      let value = matched_string input in
      Tok_Int (int_of_string value) :: helper (index + String.length value)
    else if string_match (regexp "\"[^\"]*\"") input index 
      then let value = matched_string input in
        let sanitized = String.sub value 1 (String.length value - 2) in
        Tok_String sanitized :: helper (index + String.length value)
    else if string_match (regexp "true\\|false") input index 
      then let value = matched_string input in 
        Tok_Bool (value = "true") :: helper (index + String.length value)
    else if string_match (regexp "then") input index 
      then Tok_Then :: helper (index + 4)
    else if string_match (regexp "else") input index 
      then Tok_Else :: helper (index + 4)
    else if string_match (regexp "not") input index 
      then Tok_Not :: helper (index + 3)
    else if string_match (regexp "let") input index 
      then Tok_Let :: helper (index + 3)
    else if string_match (regexp "rec") input index 
      then Tok_Rec :: helper (index + 3)
    else if string_match (regexp "def") input index 
      then Tok_Def :: helper (index + 3)
    else if string_match (regexp "fun") input index 
      then Tok_Fun :: helper (index + 3)
    else if string_match (regexp "<>") input index 
      then Tok_NotEqual :: helper (index + 2)
    else if string_match (regexp ">=") input index 
      then Tok_GreaterEqual :: helper (index + 2)
    else if string_match (regexp "<=") input index 
      then Tok_LessEqual :: helper (index + 2)
    else if string_match (regexp "||") input index 
      then Tok_Or :: helper (index + 2)
    else if string_match (regexp "&&") input index 
      then Tok_And :: helper (index + 2)
    else if string_match (regexp "if") input index 
      then Tok_If :: helper (index + 2)
    else if string_match (regexp "in") input index 
      then Tok_In :: helper (index + 2)
    else if string_match (regexp "->") input index 
      then Tok_Arrow :: helper (index + 2)
    else if string_match (regexp ";;") input index 
      then Tok_DoubleSemi :: helper (index + 2)
    else if string_match (regexp "(") input index 
      then Tok_LParen :: helper (index + 1)
    else if string_match (regexp ")") input index 
      then Tok_RParen :: helper (index + 1)
    else if string_match (regexp "{") input index 
      then Tok_LCurly :: helper (index + 1)
    else if string_match (regexp "}") input index 
      then Tok_RCurly :: helper (index + 1)
    else if string_match (regexp "\\.") input index 
      then Tok_Dot :: helper (index + 1)
    else if string_match (regexp "=") input index 
      then Tok_Equal :: helper (index + 1)
    else if string_match (regexp ">") input index 
      then Tok_Greater :: helper (index + 1)
    else if string_match (regexp "<") input index 
      then Tok_Less :: helper (index + 1)
    else if string_match (regexp "\\+") input index 
      then Tok_Add :: helper (index + 1)
    else if string_match (regexp "-") input index 
      then Tok_Sub :: helper (index + 1)
    else if string_match (regexp "\\*") input index 
      then Tok_Mult :: helper (index + 1)
    else if string_match (regexp "/") input index 
      then Tok_Div :: helper (index + 1)
    else if string_match (regexp "\\^") input index 
      then Tok_Concat :: helper (index + 1)
    else if string_match (regexp ";") input index 
      then Tok_Semi :: helper (index + 1)
    else if string_match (regexp "[a-zA-Z][a-zA-Z0-9]*") input index 
      then let value = matched_string input in
        Tok_ID value :: helper (index + String.length value)
    else if Str.string_match (regexp " ") input index 
      then helper (index + 1)
    else if Str.string_match (regexp "\t") input index 
      then helper (index + 1)
    else if Str.string_match (regexp "\n") input index 
      then helper (index + 1)
    else raise (InvalidInputException "Invalid Input")
  in helper 0

      

    

