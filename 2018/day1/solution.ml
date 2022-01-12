open Base
open Stdio

let parse_input () =
  let ic = In_channel.create "input.txt" in
  In_channel.fold_lines ic ~init:[] ~f:(fun acc line ->
      Int.of_string line :: acc
    )

let rec sum xs =
  match xs with
  | [] -> 0
  | h :: t -> h + sum t

type state =
  { seen : (Int.t, Int.comparator_witness) Set.t;
    freq : int;
  }

let empty = Set.empty (module Int)

let part2 xs =
  let rec go xxs seen freq =
    match xxs with
    | [] -> go xs seen freq
    | h :: t ->
      let freq1 = freq + h in
      if Set.exists seen ~f:(equal_int freq1) then freq1
      else go t (Set.add seen freq1) freq1
  in go xs empty 0

let () =
  let input = parse_input () in
  printf "part1 %d\n" (sum input);
  printf "part2 %d\n" (part2 (List.rev input));
