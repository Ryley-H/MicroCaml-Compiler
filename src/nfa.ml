open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  List.fold_left (fun acc q ->
    List.fold_left (fun acc (src, sym, dst) -> if src = q && sym = s then insert dst acc else acc) acc nfa.delta) [] qs

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec epsilon_helper states visited =
    match states with
    | [] -> visited
    | q :: rest ->
        if List.mem q visited then epsilon_helper rest visited
        else
          let new_visited = q :: visited in
          let epsilon_targ = List.filter (fun (src, sym, _) -> src = q && sym = None) nfa.delta in
          let next_states = List.map (fun (_, _, dst) -> dst) epsilon_targ in
          epsilon_helper(next_states @ rest) new_visited
  in
  List.rev (epsilon_helper qs [])

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec accept_helper states chars =
    match chars with
    | [] -> List.exists (fun q -> List.mem q nfa.fs) states
    | c :: rest -> let next_states = move nfa states (Some c) in
        accept_helper (e_closure nfa next_states) rest in   
        accept_helper (e_closure nfa [nfa.q0]) (explode s)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.map (fun x -> e_closure nfa (move nfa qs (Some x))) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun x -> let next_states = e_closure nfa (move nfa qs (Some x)) in (qs, Some x, next_states)) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if List.exists (fun x -> List.mem x nfa.fs) qs then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match work with
    | [] -> dfa
    | qs :: rest -> let new_trans = List.filter (fun (_, _, q) -> not (List.mem q dfa.qs)) (new_trans nfa qs) in
      let new_states = List.filter (fun q -> not (List.mem q dfa.qs)) (new_states nfa qs) in
      let new_finals = if List.exists (fun q -> List.mem q nfa.fs) qs then qs :: dfa.fs else dfa.fs in
      let new_dfa = {
        sigma = dfa.sigma;
        qs = qs :: dfa.qs;
        q0 = dfa.q0;
        fs = new_finals;
        delta = new_trans @ dfa.delta;} in nfa_to_dfa_step nfa new_dfa (rest @ new_states)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start_state = e_closure nfa [nfa.q0] in
  let dfa = { sigma = nfa.sigma; qs = []; q0 = start_state; fs = []; delta = [] } in
  nfa_to_dfa_step nfa dfa [start_state]
