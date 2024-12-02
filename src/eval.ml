open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec eval_expr env e = match e with
  | Int integer -> Int integer
  | Bool boolean -> Bool boolean
  | String str -> String str
  | ID x -> lookup env x
  | Fun (param, body) -> Closure (env, param, body)
  | Not expr -> (match eval_expr env expr with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Error"))
  | Binop (op, left, right) -> let left_val = eval_expr env left in let right_val = eval_expr env right in (match op with
    | Add -> (match (left_val, right_val) with
      | (Int l, Int r) -> Int (l + r)
      | _ -> raise (TypeError "Error"))
    | Sub -> (match (left_val, right_val) with
      | (Int l, Int r) -> Int (l - r)
      | _ -> raise (TypeError "Error"))
    | Mult -> (match (left_val, right_val) with
        | (Int l, Int r) -> Int (l * r)
        | _ -> raise (TypeError "Error"))
    | Div -> (match (left_val, right_val) with
        | (Int _, Int 0) -> raise DivByZeroError 
        | (Int l, Int r) -> Int (l / r)
        | _ -> raise (TypeError "Error"))
    | Greater -> (match (left_val, right_val) with
        | (Int l, Int r) -> Bool (l > r)
        | _ -> raise (TypeError "Error"))
    | Less -> (match (left_val, right_val) with
        | (Int l, Int r) -> Bool (l < r)
        | _ -> raise (TypeError "Error"))
    | GreaterEqual -> (match (left_val, right_val) with
        | (Int l, Int r) -> Bool (l >= r)
        | _ -> raise (TypeError "Error"))
    | LessEqual -> (match (left_val, right_val) with
        | (Int l, Int r) -> Bool (l <= r)
        | _ -> raise (TypeError "Error"))
    | Concat -> (match (left_val, right_val) with
        | (String l, String r) -> String (l ^ r)
        | _ -> raise (TypeError "Error"))
    | Equal -> Bool (match (left_val, right_val) with
        | (Int _, Int _) -> left_val = right_val
        | (Bool _, Bool _) -> left_val = right_val
        | (String _, String _) -> left_val = right_val
        | _ -> raise (TypeError "Error"))
    | NotEqual -> Bool (match (left_val, right_val) with
        | (Int _, Int _) -> left_val <> right_val
        | (Bool _, Bool _) -> left_val <> right_val
        | (String _, String _) -> left_val <> right_val
        | _ -> raise (TypeError "Error"))
    | Or -> (match (left_val, right_val) with
        | (Bool l, Bool r) -> Bool (l || r)
        | _ -> raise (TypeError "Error"))
    | And -> (match (left_val, right_val) with
        | (Bool l, Bool r) -> Bool (l && r)
        | _ -> raise (TypeError "Error")))
  | If (guard, then_branch, else_branch) -> (match eval_expr env guard with
    | Bool true -> eval_expr env then_branch
    | Bool false -> eval_expr env else_branch
    | _ -> raise (TypeError "Error"))
  | Let (var, is_rec, init, body) -> let init_val = eval_expr env init in
    if is_rec then let closure = Closure (env, var, body) in  
      let env_with_placeholder = extend env var closure in
      let init_val = eval_expr env_with_placeholder init in update env_with_placeholder var init_val; eval_expr env_with_placeholder body
    else let init_val = eval_expr env init in eval_expr (extend env var init_val) body
  | Record fields -> Record (List.map (fun (label, expr) -> (label, eval_expr env expr)) fields)
  | Select (label, record_expr) -> let record_val = eval_expr env record_expr in (match record_val with
    | Record fields -> (try List.assoc label fields with Not_found -> raise (SelectError ("Select Error")))
    | _ -> raise (TypeError "Error"))
  | App (func_expr, arg_expr) -> let func_val = eval_expr env func_expr in let arg_val = eval_expr env arg_expr in (match func_val with
    | Closure (closure_env, param, body) -> let new_env = extend closure_env param arg_val in eval_expr new_env body
    | _ -> raise (TypeError "Error"))


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  | Def (var, expr) -> let placeholder_env = extend_tmp env var in
      let ex = eval_expr placeholder_env expr in update placeholder_env var ex; (extend env var ex, Some ex)
  | Expr expr -> let result = eval_expr env expr in (env, Some result)
  | NoOp -> (env, None)
