type operation = OpAdd | OpMul | OpSub | OpDiv
type token = TokNum of int | TokOp of operation | TokLp | TokRp

exception CalcError of string

let priority = Hashtbl.create 4;;

Hashtbl.add priority OpAdd 100;;
Hashtbl.add priority OpSub 100;;
Hashtbl.add priority OpMul 200;;
Hashtbl.add priority OpDiv 200

let token_map = Hashtbl.create 6;;

Hashtbl.add token_map '+' (TokOp OpAdd);;
Hashtbl.add token_map '*' (TokOp OpMul);;
Hashtbl.add token_map '-' (TokOp OpSub);;
Hashtbl.add token_map '/' (TokOp OpDiv);;
Hashtbl.add token_map '(' TokLp;;
Hashtbl.add token_map ')' TokRp

let bin_op_exec = Hashtbl.create 4;;

Hashtbl.add bin_op_exec OpAdd ( + );;
Hashtbl.add bin_op_exec OpSub ( - );;
Hashtbl.add bin_op_exec OpMul ( * );;
Hashtbl.add bin_op_exec OpDiv ( / )

let tokenize s : token list =
  let token_builder ts c =
    match (ts, c) with
    | TokNum x :: rest, '0' .. '9' ->
        TokNum ((x * 10) + int_of_char c - 48) :: rest
    | _, '0' .. '9' -> TokNum (int_of_char c - 48) :: ts
    | _, ' ' -> ts
    | _, _ -> (
        match Hashtbl.find_opt token_map c with
        | Some t -> t :: ts
        | None -> raise (CalcError "Invalid token"))
  in
  String.fold_left token_builder [] s |> List.rev

let rec expr stack toks : int * token list =
  let reduce stack =
    match stack with
    | TokNum y :: TokOp o :: TokNum x :: ts ->
        TokNum (Hashtbl.find bin_op_exec o x y) :: ts
    | _ -> raise (CalcError "Parse Error")
  in
  match (toks, stack) with
  | [], [ TokNum x ] -> (x, [])
  | TokRp :: ts, [ TokNum x ] -> (x, ts)
  | ([] | TokRp :: _), _ :: _ -> expr (reduce stack) toks
  | TokOp a :: ts, _ :: TokOp b :: _ ->
      if Hashtbl.find priority a <= Hashtbl.find priority b then
        expr (reduce stack) toks
      else expr (TokOp a :: stack) ts
  | TokLp :: ts, _ ->
      let x, ts = expr [] ts in
      expr (TokNum x :: stack) ts
  | t :: ts, _ -> expr (t :: stack) ts
  | _ -> raise (CalcError "Parse Error")

let () =
  while true do
    Printf.printf "> ";
    try
      let x, rest = read_line () |> tokenize |> expr [] in
      if rest <> [] then failwith "Unfinished expression"
      else Printf.printf "%d\n" x
    with
    | CalcError s -> Printf.printf "ERROR: %s\n" s
    | e -> raise e
  done
