module Intcode = struct
  open Base
  open Stdio

  let parse_input_into s a =
    let ic = In_channel.create s in
    let rec go i n =
      match In_channel.input_char ic with
      | Some '0' -> go i (n * 10 + 0)
      | Some '1' -> go i (n * 10 + 1)
      | Some '2' -> go i (n * 10 + 2)
      | Some '3' -> go i (n * 10 + 3)
      | Some '4' -> go i (n * 10 + 4)
      | Some '5' -> go i (n * 10 + 5)
      | Some '6' -> go i (n * 10 + 6)
      | Some '7' -> go i (n * 10 + 7)
      | Some '8' -> go i (n * 10 + 8)
      | Some '9' -> go i (n * 10 + 9)
      | Some ',' -> Array.set a i n; go (i + 1) 0
      | Some '\n' -> go i 0
      | None -> Array.set a i n
      | Some c -> failwith (Printf.sprintf "invalid input char %c" c)
    in go 0 0

  type instruction =
    | Add of (int * int * (int -> unit))
    | Mul of (int * int * (int -> unit))
    | Halt

  type trap =
    | Halt

  let param a ip n =
    let get = Array.get a in
    let mode = 0 in
    match mode with
    | 0 -> get (get (ip + n))
    | _ -> failwith (Printf.sprintf "invalid param mode %d" mode)

  let result a ip n =
    let get = Array.get a in
    let set = Array.set a in
    let mode = 0 in
    match mode with
    | 0 -> set (get (ip + n))
    | _ -> failwith (Printf.sprintf "invalid result mode %d" mode)

  let decode a ip =
    let opcode = Array.get a ip in
    let p = param a ip in
    let r = result a ip in
    match opcode with
    | 1  -> Add (p 1, p 2, r 3)
    | 2  -> Mul (p 1, p 2, r 3)
    | 99 -> Halt
    | _  -> failwith (Printf.sprintf "opcode %d" opcode)

  type machine =
    { mutable trap : trap option
    }

  let execute state ins =
    match ins with
    | Add (a, b, r) -> r (a + b)
    | Mul (a, b, r) -> r (a * b)
    | Halt -> state.trap <- Some Halt

  let run_until_halt mem =
    let state = { trap = None } in
    let rec step ip =
      ip
      |> decode mem
      |> execute state;
      match state with
      | { trap = None } -> step (ip + 4)
      | _               -> ()
    in step 0

  let run_until_halt_args mem ~noun ~verb =
    let mem' = (Array.copy mem) in
    Array.set mem' 1 noun;
    Array.set mem' 2 verb;
    run_until_halt mem';
    Array.get mem' 0
end
