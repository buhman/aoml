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

let part2 xs =
  let seen = Hashtbl.create (module Int) in
  let rec go xxs freq =
    match xxs with
    | [] -> go xs freq
    | h :: t ->
      let freq1 = freq + h in
      if Hashtbl.mem seen freq1 then freq1
      else
        let _ = Hashtbl.add seen ~key:freq1 ~data:1 in
        go t freq1
  in go xs 0

let () =
  let input = parse_input () in
  printf "part1 %d\n" (sum input);
  printf "part2 %d\n" (part2 (List.rev input));
