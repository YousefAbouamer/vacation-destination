open Game.Adventure
(** [play_game f] starts the adventure in file [f]. *)
open Game.Command
open Game.State

let rec play_helper adven state=
  let state = init_state adven in match read_line () with
  | exception End_of_file -> ()
  | input_name -> match parse input_name with 
    |exception Malformed -> print_endline("You typed in the an invalid command. Retype a valid command"); play_helper adven state
    |exception Empty -> print_endline ("You did not type anything, go ahead and retype a valid command"); play_helper adven state
    |x-> match x with 
    |Quit-> print_endline ("Thanks for playing. See you next time! ") 
    | Go t-> match go (String.concat " " t) adven state  with  
    |Illegal-> print_endline ("Your result is illegal. Try again!"); play_helper adven state
    |Legal t-> print_endline (description adven (current_room_id state)); play_helper adven state


let play_game f = let json=  Yojson.Basic.from_file f in 
  let adven =  from_json json in
  let state = init_state adven in play_helper adven state 

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
