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

let start_room (adv : t) = adv.strt_room
let room_ids adv = raise (Failure "Unimplemented: Adventure.room_ids")

let description adv room =
  raise (Failure "Unimplemented: Adventure.description")

let exits adv room = raise (Failure "Unimplemented: Adventure.exits")
let next_room adv room ex = raise (Failure "Unimplemented: Adventure.next_room")
let next_rooms adv room = raise (Failure "Unimplemented: Adventure.next_rooms")
