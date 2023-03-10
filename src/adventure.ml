open Yojson.Basic.Util

exception UnknownRoom of string
exception UnknownExit of string

type ext = {
  name : string;
  roomid : string;
}

type rm = {
  i : string;
  desc : string;
  exitt : ext list;
}

type t = {
  rms : rm list;
  strt_room : string;
}

let exit_from_json (json : Yojson.Basic.t) : ext =
  {
    name = json |> member "name" |> to_string;
    roomid = json |> member "room id" |> to_string;
  }

let from_json json =
  let room_of_json json : rm =
    {
      i = json |> member "id" |> to_string;
      desc = json |> member "description" |> to_string;
      exitt = json |> member "exits" |> to_list |> List.map exit_from_json;
    }
  in
  let start_room_of_json = json |> member "start room" |> to_string in
  let rooms_of_json =
    {
      rms = json |> member "rooms" |> to_list |> List.map room_of_json;
      strt_room = start_room_of_json;
    }
  in
  { rms = rooms_of_json.rms; strt_room = start_room_of_json }

let start_room adv = adv.strt_room
let room_ids adv = List.map (fun x -> x.i) adv.rms

let rec description adv room =
  match adv.rms with
  | [] -> raise (UnknownRoom room)
  | h :: t -> if h.i = room then h.desc else description adv h.i

let rec exits_helper rms room =
  match rms with
  | [] -> raise (UnknownRoom room)
  | h :: t ->
      if h.i = room then List.map (fun x -> x.name) h.exitt
      else exits_helper t room

let rec exits adv room = exits_helper adv.rms room

let next_room adv room ex =
  let filter_list = List.filter (fun x -> x.i = room) adv.rms in
  match filter_list with
  | [] -> raise (UnknownRoom room)
  | h2 :: t2 -> (
      let filter_list2 = List.filter (fun x -> x.name = ex) h2.exitt in
      match filter_list2 with
      | [] -> raise (UnknownExit ex)
      | h :: t -> h.roomid)

let next_rooms adv room = raise (Failure "Unimplemented: Adventure.next_rooms")
