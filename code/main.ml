open Hand

(* The actual implementation of the Japanese Mahjong Hand Helper application.
 * Use the hand module. *)

(* Determine whether the hand is winning hand or not using the backtracking algorithm. *)
let rec game turn win hand wall =
  if !win == 0 then (
    if (Hand.backtrack hand (ref 0)) then (
      print_string "\nWin! -> ";
      Hand.print_tiles hand;
      Hand.draw_hand hand;
      Hand.unmark hand;
      win := 1;
      print_string "\n"
      )
    else (
      if turn = -1 then print_string "Lose\n" 
      else (
        Hand.discard hand;
        Hand.draw (turn) wall hand;
        Hand.print_tiles hand;
        Hand.draw_hand hand;
        print_string "\n";
        game (turn - 1) win hand wall
        )
      )
    )

(* Start the game. Initialize the wall and hand.
 * When the game is over, prompt the user to restart. *)
let rec game_start str =
  print_string "Game Start!\n";
  Graphics.open_graph "";
  (* Sample short wall used in demo video. Uncomment to use. 
   * If used, make sure to comment the next generate_wall call. *)
  (*
  let wall = Hand.generate_wall_test in
  *)
  let wall = Hand.generate_wall in
  Hand.shuffle wall;
  let hand = Hand.generate_hand wall in
  Hand.sort hand;
  Hand.print_tiles hand;
  Hand.draw_hand hand;
  print_string "\n";
  let win = ref 0 in
  let turn = Array.length wall - 15 in
  game turn win hand wall;
  Printf.printf "> Hit y + Enter to restart! : %!";
  let x = read_line() in
  if String.compare x str = 0 then game_start "y"
  else print_string "Thanks for playing!\n"

(* Open the game and prompt the user to start. *)
let start_ui =
  print_string "~Japanese Mahjong Hand Helper v1.0~\n";
  print_string "Hit y + Enter to start!\n";
  let x = read_line() in
  if String.compare x "y" = 0 then game_start "y"
