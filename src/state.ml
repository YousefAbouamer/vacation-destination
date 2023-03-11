open Adventure

type t = {
  room_id : string;
  visited_list : string list;
}

let init_state adv =
  { room_id = start_room adv; visited_list = [ start_room adv ] }

let current_room_id st = st.room_id
let visited st = List.sort_uniq String.compare st.visited_list

type result =
  | Legal of t
  | Illegal

let go ex adv st = raise (Failure "Not Implemented")
