(* Hand module:
 * Define the tiles and hand along with various functions and tests. *)
module type HAND =
sig
  type num
  type suit
  type honor
  type flag
  type tile
  type hand
  
  val string_of_num : num -> string
  val int_of_num : num -> int
  val string_of_suit : suit -> string
  val string_of_honor : honor -> string
  val print_tile : tile -> unit
  val print_tiles : tile array -> unit
  
  val generate_wall : tile array
  (* Sample short wall used in demo video. Uncomment to use. *)
  (*
  val generate_wall_test : tile array
  *)
  val shuffle : tile array -> unit
  val generate_hand : tile array -> tile array

  val sort : tile array -> unit
  val draw : int -> tile array -> tile array -> unit
  val discard : tile array -> unit
  
  val track_pair : tile array -> int -> unit
  val track_set : tile array -> unit

  val verify : tile array -> bool
  val unmark : tile array -> unit
  val backtrack : tile array -> int ref -> bool
  
  val draw_tile : tile -> int * int -> unit
  val draw_hand : tile array -> unit
  
  (* Uncomment to run tests. *)
  (*
  val print_test : tile array -> unit
  val run_tests : unit
  *)
end

module Hand : HAND =
struct
  
  (* All the numbers for tiles. 'Ten' is used for honor tiles. *)
  type num = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  
  (* All the suits for tiles. 'Hon' is used for honor tiles. *)  
  type suit = Pin | Sou | Man | Hon
  
  (* All the honor tiles. 'Nope' is used for num+suit tiles. *)
  type honor = East | South | West | North | White | Green | Red | Nope
  
  (* Used for marking tiles in backtrack. *)
  type flag = True | False
  
  (* A tile is defined as a record with type num, suit, honor, and flag. *)
  type tile = {num:num; suit:suit; honor:honor; mutable flag:flag}
  
  (* A hand is defined as an array of tiles. *)
  type hand = tile array
  
  (* Convert type num into a string.
   * For honor tiles there is no num, so "" is used. *) 
  let string_of_num n = 
    match n with
    | One -> "1"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> ""
  
  (* Convert type num into an int.
   * For honor tiles there is no num, so '10' is used. *)
  let int_of_num n = 
    match n with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
  
  (* Convert type suit into a string.
   * For honor tiles there is no suit, so 'z' is used. *)
  let string_of_suit s =
    match s with
    | Sou -> "s"
    | Pin -> "p"
    | Man -> "m"
    | Hon -> "z"
  
  (* Convert type honor into a string.
   * For num+suit tiles, there is no honor, so "" is used. *)
  let string_of_honor s =
    match s with
    | East -> "E"
    | South -> "S"
    | West -> "W"
    | North -> "N"
    | White -> "H"
    | Green -> "G"
    | Red -> "R"
    | Nope -> ""
  
  (* Print number and suit for a num+suit tile.
   * Print honor for an honor tile. *)
  let print_tile tile =
    print_string (string_of_num(tile.num));
    if (String.compare (string_of_suit(tile.suit)) "p" = 0)
    || (String.compare (string_of_suit(tile.suit)) "s" = 0)
    || (String.compare (string_of_suit(tile.suit)) "m" = 0) then       
    print_string (string_of_suit(tile.suit));
    print_string (string_of_honor(tile.honor));
    print_string " "
  
  (* Print all the tiles in a hand. *)
  let print_tiles (hand : tile array) =
    for i = Array.length hand - 1 downto 0 do
      print_tile hand.(i)
    done
  
  (* Generate the wall where player draws the tiles from. *)
  let generate_wall =
    (* num+suit tiles *)
    let nums = [One; Two; Three; Four; Five; Six; Seven; Eight; Nine] in
    let suits = [Pin; Sou; Man] in
    let honor = Nope in
    let flag = False in
    let nsTiles1 = Array.of_list (List.concat (List.map (fun num -> List.map
      (fun suit -> {num;suit;honor;flag}) suits) nums)) in
    let nsTiles2 = Array.of_list (List.concat (List.map (fun num -> List.map
      (fun suit -> {num;suit;honor;flag}) suits) nums)) in
    let nsTiles3 = Array.of_list (List.concat (List.map (fun num -> List.map
      (fun suit -> {num;suit;honor;flag}) suits) nums)) in
    let nsTiles4 = Array.of_list (List.concat (List.map (fun num -> List.map
      (fun suit -> {num;suit;honor;flag}) suits) nums)) in
    (* honor tiles *)
    let num = Ten in
    let suit = Hon in
    let honors = [East; South; West; North; White; Green; Red] in
    let hTiles1 = Array.of_list (List.map (fun honor ->
      {num;suit;honor;flag}) honors) in
    let hTiles2 = Array.of_list (List.map (fun honor ->
      {num;suit;honor;flag}) honors) in
    let hTiles3 = Array.of_list (List.map (fun honor ->
      {num;suit;honor;flag}) honors) in
    let hTiles4 = Array.of_list (List.map (fun honor ->
      {num;suit;honor;flag}) honors) in
    (* Combine num+suit tiles and honor tiles. *)
    Array.concat [nsTiles1; nsTiles2; nsTiles3; nsTiles4;
                  hTiles1; hTiles2; hTiles3; hTiles4]
  
  (* Sample short wall used in demo video. Uncomment to use. *)
  (*
  let generate_wall_test =
    Array.of_list (List.rev [
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Six;suit=Man;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Eight;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Sou;honor=Nope;flag=False};
      {num=Four;suit=Sou;honor=Nope;flag=False};
      {num=Ten;suit=Hon;honor=West;flag=False};
      {num=Six;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Sou;honor=Nope;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False}])
  *)
  
  (* Fisher-Yates shuffle for shuffling the wall.
   * http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)
  let shuffle (w : tile array) =
    for i = Array.length w - 1 downto 0 do
      let j = Random.int (i+1) in
      let temp = w.(i) in
      w.(i) <- w.(j);
      w.(j) <- temp
    done
  
  (* Generate a hand based on the wall. *)
  let generate_hand (w : tile array) =
    Array.of_list (List.rev [w.((Array.length w - 1)); w.((Array.length w - 2)); w.((Array.length w - 3));
      w.((Array.length w - 4)); w.((Array.length w - 5)); w.((Array.length w - 6));
      w.((Array.length w - 7)); w.((Array.length w - 8)); w.((Array.length w - 9));
      w.((Array.length w - 10)); w.((Array.length w - 11)); w.((Array.length w - 12));
      w.((Array.length w - 13)); w.((Array.length w - 14))])
  
  (* Sort a hand. The rule for sorting a hand from left to right is
   * 1-9m, 1-9p, 1-9s, E, S, W, N, H, G, R. *) 
  let sort (h : tile array) =
    for i = Array.length h - 1 downto 0 do
      for j = i - 1 downto 0 do
        let tile1 = h.(i) in
        let tile2 = h.(j) in
        if String.compare (string_of_suit(tile1.suit)) (string_of_suit(tile2.suit)) = 1 then
          let suits = ((tile1.suit), (tile2.suit)) in
          match suits with
          | (Man, _) -> ()
          | (Pin, Man) -> (Array.set h i tile2; Array.set h j tile1)
          | (Pin, _) -> ()
          | (Sou, (Man|Pin)) -> (Array.set h i tile2; Array.set h j tile1)
          | (Sou, _) -> ()
          | (Hon, (Man|Pin|Sou)) -> (Array.set h i tile2; Array.set h j tile1)
          | (Hon, _) -> ()
        else if String.compare (string_of_suit(tile1.suit)) (string_of_suit(tile2.suit)) = 0 then
          if int_of_num(tile2.num) < int_of_num(tile1.num) then (
            Array.set h i tile2; Array.set h j tile1)
          else if int_of_num(tile2.num) == int_of_num(tile1.num) then (
            let honors = ((tile1.honor), (tile2.honor)) in
            match honors with
            | (East, _) -> ()
            | (South, East) -> (Array.set h i tile2; Array.set h j tile1)
            | (South, _) -> ()
            | (West, (East|South)) -> (Array.set h i tile2; Array.set h j tile1)
            | (West, _) -> ()
            | (North, (East|South|West)) -> (Array.set h i tile2; Array.set h j tile1)
            | (North, _) -> ()
            | (White, (East|South|West|North)) -> (Array.set h i tile2; Array.set h j tile1)
            | (White, _) -> ()
            | (Green, (East|South|West|North|White)) -> (Array.set h i tile2; Array.set h j tile1)
            | (Green, _) -> ()
            | (Red, (East|South|West|North|White|Green)) -> (Array.set h i tile2; Array.set h j tile1)
            | (_, _) -> (Array.set h i tile2; Array.set h j tile1))
      done
    done
  
  (* Draw a tile from the wall into the hand. *)
  let draw (c : int) (w : tile array) (h : tile array) = 
    let draw_tile = w.(c) in
    Array.set h 0 draw_tile
  
  (* Discard a tile from the hand. Prompt the user in the terminal, read the user
   * input, and match to see if the hand has the specified tile. If no match, prompt
   * the user to try again. Uncomment below to see the new hand in the terminal. *)
  let rec discard (h : tile array) =
    Printf.printf "> Discard: %!";
    let tile2 = read_line() in
    let a = ref 0 in
    for i = Array.length h - 1 downto 0 do
      let tile1 = h.(i) in
      (match tile2 with
      | "E" | "W" | "S" | "N" | "H" | "G" | "R" ->
        (if String.compare (string_of_honor(tile1.honor)) (Char.escaped (String.get tile2 0)) = 0
        && !a == 0 then
          (Array.set h i {num=Ten;suit=Hon;honor=Nope;flag=False}; a := 1))
      | "1p" | "2p" | "3p" | "4p" | "5p" | "6p" | "7p" | "8p" | "9p" |
        "1m" | "2m" | "3m" | "4m" | "5m" | "6m" | "7m" | "8m" | "9m" |
        "1s" | "2s" | "3s" | "4s" | "5s" | "6s" | "7s" | "8s" | "9s" ->
        (if String.compare (string_of_suit(tile1.suit)) (Char.escaped (String.get tile2 1)) = 0
          && String.compare (string_of_num(tile1.num)) (Char.escaped (String.get tile2 0)) = 0
          && !a == 0 then
          (Array.set h i {num=Ten;suit=Hon;honor=Nope;flag=False}; a := 1))
      | _ -> ())
    done ;
    if !a == 0 then (print_string "Try again\n"; discard h)
    else (sort h(*; print_tiles h; print_string "\n"*))
  
  (* Track to see if there is a pair and mark the tiles if so.
   * Uncomment below to see the tracked pair in the terminal. *)
  let track_pair (h : tile array) (c : int) =
    let a = ref 0 in
    for i = Array.length h - 1 - c downto 0 do
      for j = i - 1 downto 0 do
        let tile1 = h.(i) in
        let tile2 = h.(j) in
        if String.compare (string_of_suit(tile1.suit)) (string_of_suit(tile2.suit)) = 0
          && int_of_num(tile1.num) == int_of_num(tile2.num)
          && String.compare (string_of_honor(tile1.honor)) (string_of_honor(tile2.honor)) = 0
          && tile1.flag = False && tile2.flag = False && !a == 0 then
          (tile1.flag <- True; tile2.flag <- True; a := 1(*;
          print_string "Pair: "; print_tile tile1; print_tile tile2*))
      done
    done
  
  (* Track to see if there are triples and/or sequences and mark the tiles if so.
   * Uncomment below to see the tracked triples/sequences in the terminal. *)
  let track_set (h : tile array) =
    for i = Array.length h - 1 downto 0 do
      for j = i - 1 downto 0 do
        for k = j - 1 downto 0 do
          let tile1 = h.(i) in
          let tile2 = h.(j) in
          let tile3 = h.(k) in
          if tile1.flag = False && tile2.flag = False && tile3.flag = False then
            if String.compare (string_of_suit(tile1.suit)) (string_of_suit(tile2.suit)) = 0
              && String.compare (string_of_suit(tile1.suit)) (string_of_suit(tile3.suit)) = 0 then (
              let triple = (int_of_num(tile1.num) == int_of_num(tile2.num)
                && int_of_num(tile1.num) == int_of_num(tile3.num)
                && String.compare (string_of_honor(tile1.honor)) (string_of_honor(tile2.honor)) = 0
                && String.compare (string_of_honor(tile1.honor)) (string_of_honor(tile3.honor)) = 0) in
              let sequence = (int_of_num(tile1.num)+1) == int_of_num(tile2.num)
                && (int_of_num(tile1.num)+2) == int_of_num(tile3.num) in
              if triple = true || sequence = true then
                (tile1.flag <- True; tile2.flag <- True; tile3.flag <- True(*;
                if triple = true then (print_string "Triple: "; print_tile tile1;
                  print_tile tile2; print_tile tile3)
                else (print_string "Sequence: "; print_tile tile1;
                  print_tile tile2; print_tile tile3)*)))
        done
      done
    done
  
  (* Verify whether all the tiles in the hand is marked or not. *)
  let verify (h : tile array) =
    if (h.(1)).flag = True && (h.(2)).flag = True && (h.(3)).flag = True && (h.(4)).flag = True
      && (h.(5)).flag = True && (h.(6)).flag = True && (h.(7)).flag = True && (h.(8)).flag = True
      && (h.(9)).flag = True && (h.(10)).flag = True && (h.(11)).flag = True && (h.(12)).flag = True
      && (h.(13)).flag = True && (h.(0)).flag = True then true else false
  
  (* Unmark all the tiles in the hand. *) 
  let unmark (h : tile array) = 
    (h.(1)).flag <- False; (h.(2)).flag <- False; (h.(3)).flag <- False; (h.(4)).flag <- False
    ; (h.(5)).flag <- False ; (h.(6)).flag <- False ; (h.(7)).flag <- False ; (h.(8)).flag <- False
    ; (h.(9)).flag <- False ; (h.(10)).flag <- False ; (h.(11)).flag <- False ; (h.(12)).flag <- False
    ; (h.(13)).flag <- False ; (h.(0)).flag <- False
  
  (* Backtracking algorithm:
   * Sort the hand and track for a pair and following triples/sequences.
   * Then verify the hand and return true if it is a winning hand.
   * Else repeat process until it either returns true or false (not a winning hand). *)
  let rec backtrack (h : tile array) (c : int ref) =
    ignore(sort h;
    track_pair h !c;
    track_set h);
    let winning_hand = verify h in
    if winning_hand = true then true
    else if !c < 14 then (unmark h; c := !c + 1; backtrack h c)
    else false
  
  (* Draw circles. *)
  let circle ((x,y):int*int) (width:int) (height:int)
             (bg:Graphics.color) (fg:Graphics.color) (t:string) : unit =
    Graphics.set_color bg ;
    Graphics.fill_circle (x*width + width/2) (y*height + height/2)
                         (min width height / 2) ;
    Graphics.moveto (x*width+2) (y*height) ;
    Graphics.set_color fg ;
    Graphics.set_text_size 10;
    Graphics.draw_string t
  
  (* Draw circles with different colors based on the tile type. *) 
  let draw_tile (t : tile) ((x,y) : (int * int)) =
    let tile_num = (string_of_num(t.num)) in   
    let tile_suit = (string_of_suit(t.suit)) in
    let tile_honor = (string_of_honor(t.honor)) in
    if String.compare tile_suit "z" = 0 then (
      match tile_honor with
      | "E" | "S" | "W" | "N" -> 
        circle (x,y) 15 15 Graphics.blue Graphics.white tile_honor
      | "H" -> circle (x,y) 15 15 Graphics.white Graphics.black tile_honor
      | "G" -> circle (x,y) 15 15 Graphics.green Graphics.black tile_honor
      | "R" -> circle (x,y) 15 15 Graphics.red Graphics.white tile_honor
      | _ -> circle (x,y) 15 15 Graphics.white Graphics.black tile_honor)
    else (
      let tile_n = String.get tile_num 0 in
      let tile_s = String.get tile_suit 0 in
      let tile = Bytes.create 2 in
      Bytes.set tile 0 tile_n; Bytes.set tile 1 tile_s;
      match tile_suit with
      | "m" -> circle (x,y) 15 15 Graphics.yellow Graphics.black tile
      | "p" -> circle (x,y) 15 15 Graphics.cyan Graphics.black tile
      | "s" -> circle (x,y) 15 15 Graphics.magenta Graphics.white tile
      | _ -> circle (x,y) 15 15 Graphics.white Graphics.black tile)
  
  (* Draw the hand. *)
  let draw_hand (h : tile array) =
    let c = ref 0 in
    for i = Array.length h - 1 downto 0 do
      draw_tile h.(i) (!c, 0);
      c := !c + 1
    done
  
  (* Uncomment to run tests. *)
  (*
  (* Test for printing unsorted/sorted hands and backtracking. *)
  let print_test (h : tile array) =
    print_string "\n---------------------------------------------------\n"; 
    print_string "  Unsorted hand: "; print_tiles h; print_string "\n";
    sort h;
    print_string "    Sorted hand: "; print_tiles h; print_string "\n";
    print_string "      Backtrack: ";
    if backtrack h (ref 0) then print_string "Win" else print_string "Lose";
    print_string "\n---------------------------------------------------"
  
  (* Test for the entire module. *)
  let run_tests =
    print_string "\n---------------- Tile test ------------------------\n";
    let tile1 = {num=One;suit=Pin;honor=Nope;flag=False} in
    let tile2 = {num=Two;suit=Sou;honor=Nope;flag=False} in
    let tile3 = {num=Three;suit=Man;honor=Nope;flag=False} in
    let tile4 = {num=Ten;suit=Hon;honor=East;flag=False} in
    print_tile tile1;
    print_tile tile2;
    print_tile tile3;
    print_tile tile4;
    print_string "\n---------------- Wall test ------------------------\n";
    print_string "Wall: "; print_tiles generate_wall; print_string "\n";
    let wall = generate_wall in
    print_string "Shuffled wall: "; shuffle wall; print_tiles wall; print_string "\n";
    print_string "\n---------------- Hand test ------------------------\n";
    let hand1 = Array.of_list (List.rev [
      {num=One;suit=Sou;honor=Nope;flag=False};
      {num=Two;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Sou;honor=Nope;flag=False};
      {num=One;suit=Sou;honor=Nope;flag=False};
      {num=Two;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Sou;honor=Nope;flag=False};
      {num=Eight;suit=Sou;honor=Nope;flag=False};
      {num=Eight;suit=Sou;honor=Nope;flag=False};
      {num=Eight;suit=Sou;honor=Nope;flag=False};
      {num=One;suit=Sou;honor=Nope;flag=False};
      {num=Two;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Sou;honor=Nope;flag=False};
      {num=Nine;suit=Sou;honor=Nope;flag=False};
      {num=Nine;suit=Sou;honor=Nope;flag=False}]) in
    let hand2 = Array.of_list (List.rev [
      {num=Four;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Sou;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Nine;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Nine;suit=Man;honor=Nope;flag=False};
      {num=Eight;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Sou;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Nine;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Sou;honor=Nope;flag=False}]) in
    let hand3 = Array.of_list (List.rev [
      {num=Four;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Pin;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False};
      {num=Nine;suit=Sou;honor=Nope;flag=False};
      {num=Eight;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Pin;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Nine;suit=Sou;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Five;suit=Sou;honor=Nope;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False}]) in
    let hand4 = Array.of_list (List.rev [
      {num=Ten;suit=Hon;honor=Green;flag=False};
      {num=One;suit=Sou;honor=Nope;flag=False};
      {num=One;suit=Pin;honor=Nope;flag=False};
      {num=Five;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Sou;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False};
      {num=Eight;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Pin;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Ten;suit=Hon;honor=White;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Seven;suit=Sou;honor=Nope;flag=False};
      {num=Ten;suit=Hon;honor=South;flag=False}]) in
    let hand5 = Array.of_list (List.rev [
      {num=Ten;suit=Hon;honor=Green;flag=False};
      {num=Ten;suit=Hon;honor=Red;flag=False};
      {num=Ten;suit=Hon;honor=White;flag=False};
      {num=Ten;suit=Hon;honor=South;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False};
      {num=Ten;suit=Hon;honor=West;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False};
      {num=Ten;suit=Hon;honor=North;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False};
      {num=Ten;suit=Hon;honor=Red;flag=False};
      {num=Ten;suit=Hon;honor=East;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=One;suit=Sou;honor=Nope;flag=False}]) in
    let hand6 = Array.of_list (List.rev [
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Man;honor=Nope;flag=False};
      {num=Six;suit=Man;honor=Nope;flag=False};
      {num=Seven;suit=Sou;honor=Nope;flag=False};
      {num=Eight;suit=Sou;honor=Nope;flag=False};
      {num=Nine;suit=Sou;honor=Nope;flag=False};
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False}]) in
    let hand7 = Array.of_list (List.rev [
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False};
      {num=Three;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False}]) in
    let hand8 = Array.of_list (List.rev [
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=One;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Man;honor=Nope;flag=False};
      {num=Five;suit=Man;honor=Nope;flag=False}]) in
    let hand9 = Array.of_list (List.rev [
      {num=Two;suit=Man;honor=Nope;flag=False};
      {num=Three;suit=Man;honor=Nope;flag=False};
      {num=Four;suit=Man;honor=Nope;flag=False};
      {num=Six;suit=Man;honor=Nope;flag=False};
      {num=Seven;suit=Man;honor=Nope;flag=False};
      {num=Eight;suit=Man;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Pin;honor=Nope;flag=False};
      {num=Four;suit=Sou;honor=Nope;flag=False};
      {num=Five;suit=Sou;honor=Nope;flag=False};
      {num=Six;suit=Sou;honor=Nope;flag=False};
      {num=Two;suit=Pin;honor=Nope;flag=False}]) in
    print_test hand1;
    print_test hand2;
    print_test hand3;
    print_test hand4;
    print_test hand5;
    print_test hand6;
    print_test hand7;
    print_test hand8;
    print_test hand9;
    print_string "\n------------ Draw/Discard test --------------------\n";
    draw (Array.length wall - 1) wall hand8;
    print_tiles hand8; print_string "\n";
    draw (Array.length wall - 2) wall hand8;
    print_tiles hand8; print_string "\n";
    discard hand8;
    draw (Array.length wall - 45) wall hand8;
    print_tiles hand8; print_string "\n";
    print_test hand8;
    print_string "\n--------------- Graphics test ---------------------\n";
    Graphics.open_graph "";    
    draw_tile tile1 (0,1);
    draw_tile tile2 (0,2);
    draw_tile tile3 (0,3);
    draw_tile tile4 (0,4);
    draw_hand hand9;
    print_string "\n---------------- Test Done ------------------------\n";
    ()
  *)
end
