open Base
open Stdio
open Intcode

let rec range a b =
  if a > b then []
  else a :: (range (a + 1) b)

let cartesian a b =
  List.concat (List.map ~f:(fun x -> List.map ~f:(fun y -> (x,y)) b) a)

let part1 mem =
  Intcode.run_until_halt_args mem ~noun:12 ~verb:2

let part2 mem =
  let expected = 19690720 in
  let rec go n_v =
    match n_v with
    | (noun, verb) :: rest ->
      let output = Intcode.run_until_halt_args mem ~noun:noun ~verb:verb in
      if equal_int output expected then noun * 100 + verb
      else go rest
    | _ -> failwith (Printf.sprintf "%d not found" expected)
  in go (cartesian (range 0 99) (range 0 99))

let main () =
  let mem = Array.create ~len:512 0 in
  Intcode.parse_input_into "input.txt" mem;
  printf "part1 %d\n" (part1 mem);
  printf "part2 %d\n" (part2 mem);
