open OUnit2
open Game
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
       ["foo"]); *);
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests. You can also use the JSON files in the data directory
   as tests. And you can add JSON files in this directory and use them, too. *)

(* Here is an example of how to load files from the data directory: *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let lonely = Yojson.Basic.from_file (data_dir_prefix ^ "lonely_room.json")
let ho = Yojson.Basic.from_file (data_dir_prefix ^ "ho_plaza.json")

(* You should not be testing any helper functions here. Test only the functions
   exposed in the [.mli] files. Do not expose your helper functions. See the
   handout for an explanation. *)

(* TODO: add unit tests for modules below. You are free to reorganize the
   definitions below. Just keep it clear which tests are for which modules. *)

let start_room_test (name : string) (adv : Adventure.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (start_room adv)

let room_ids_test (name : string) (adv : Adventure.t)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (room_ids adv)

let description_test (name : string) (adv : Adventure.t) (room : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (description adv room) ~printer:String.escaped

let exits_test (name : string) (adv : Adventure.t) (room : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (exits adv room) ~printer:(pp_list pp_string)

let exits_test_empty_list (name : string) (adv : Adventure.t) (room : string)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_raises (UnknownRoom room) (fun () -> exits adv room)

let next_room_test (name : string) (adv : Adventure.t) (room : string)
    (ex : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (next_room adv room ex) ~printer:String.escaped

let next_rooms_test (name : string) (adv : Adventure.t) (room : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (next_rooms adv room)
    ~printer:(pp_list pp_string)

let adventure_tests =
  [
    start_room_test "Testing Ho plaza" (from_json ho) "ho plaza";
    room_ids_test "Testing Ho plaza room ids" (from_json ho)
      [ "health"; "ho plaza"; "nirvana"; "tower" ];
    description_test "Testing Ho plaza description " (from_json ho) "ho plaza"
      "You are on Ho Plaza. Cornell Health is to the southwest. The chimes are \
       playing a concert in the clock tower. Someone tries to hand you a \
       quartercard, but you avoid them.";
    exits_test "Testing Ho plaza exits" (from_json ho) "health"
      [ "Ho Plaza"; "north east"; "northeast" ];
    exits_test "testing empty exit list" (from_json ho) "nirvana" [];
    next_room_test "Testing Ho plaza " (from_json ho) "health" "northeast"
      "ho plaza";
    next_rooms_test "Testing Ho plaza " (from_json ho) "ho plaza"
      [ "health"; "tower" ];
  ]

let parse_test (name : string) (str : string) (expected_output : command) : test
    =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_malformed_test (name : string) (str : string) : test =
  name >:: fun _ -> assert_raises Malformed (fun () -> parse str)

let command_tests =
  [
    parse_test "Testing quit command" "quit" Quit;
    parse_test "Testing the valid command" "    go   clock   tower   "
      (Go [ "clock"; "tower" ]);
    parse_malformed_test "Testing the case with an invalid command " " go   ";
    parse_malformed_test "Testing the case with an invalid command with no  go"
      "quit clock tower";
  ]

let current_room_id_test (name : string) (st : State.t) (room_id : string)
    (visited : string list) (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (current_room_id st)

let state_tests =
  [
    current_room_id_test "Testing Ho plaza "
      (init_state (from_json ho))
      "ho plaza" [] "ho plaza";
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ cmp_demo; adventure_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
