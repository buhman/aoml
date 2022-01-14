open Base
open Stdio

let map_incr m k =
  Map.update m k ~f:(fun v -> match v with
      | Some n -> n + 1
      | None -> 1
    )

let count_letters s =
  let empty = Map.empty (module Char) in
  String.fold s ~init:empty ~f:map_incr

let invert m =
  Map.fold m ~init:(false, false) ~f:(fun ~key ~data (two, three) ->
      match data with
      | 2 -> (true, three)
      | 3 -> (two, true)
      | _ -> (two, three)
    )

let count_tt xs =
  let maybe_inc i b = if b then i + 1 else i in
  List.fold xs ~init:(0, 0) ~f:(fun (twos, threes) (two, three) ->
      ((maybe_inc twos two), (maybe_inc threes three))
    )

let part1 input =
  let (a, b) =
    input
    |> List.map ~f:(Fn.compose invert count_letters)
    |> count_tt
  in a * b

let parse_input () =
  let ic = In_channel.create "input.txt" in
  In_channel.input_lines ic

let compare a b =
  let al = String.to_list a
  and bl = String.to_list b in
  let c = List.zip_exn al bl in
  List.fold c ~init:[] ~f:(fun a (q, r) -> if Char.equal q r then (q :: a) else a)

let filter_ids xs q =
  let correct = (String.length q) - 1 in
  List.filter xs ~f:(fun x -> Int.equal (List.length (compare q x)) correct)

let part2 xs =
  let filtered = List.map xs ~f:(filter_ids xs) in
  match List.concat filtered with
  | a::b::[] ->
    let answer = String.of_char_list (List.rev (compare a b))
    in answer
  | _ -> ""

let () =
  let input = parse_input () in
  printf "part1 %d\n" (part1 input);
  printf "part2 %s\n" (part2 input)
