(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str =
  let str_list = String.split_on_char ' ' str in
  let filter_list = List.filter (fun x -> x <> "") str_list in
  match filter_list with
  | [] -> raise Empty
  | [ "go" ] -> raise Malformed
  | "go" :: t -> Go t
  | [ "quit" ] -> Quit
  | "quit" :: t -> raise Malformed
  | _ :: t -> raise Malformed
