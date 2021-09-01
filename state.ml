open Monopoly


(** The state of a monopoly game is defined as a current player, a boolean
    to tell whether the current player has rolled, an int representing the
    number of players left in the game, a list of eliminated players,
    an association list mapping each player to their position on the
    board, an association list mapping each player to the amount of money
    they have, an association list mapping each player to a list of properties
    owned by that player, an association list mapping each tile to 
    that tile's owner, an association list mapping the number of houses to a tile, 
    an association mapping the number of houses and hotels to a player, 
    an association list mapping whether a player owns a get out of jail card, 
    an association list mapping whether the player is in jail, and an association
    list mapping the number of turns the player is in jail. For tile owners, -1 
    means the tile cannot be bought, 0 means the tile can be bought but has 
    no owner, and  a number 1-4 means the tile is owned by the 
    respective player 1-4.*)
type t = 
  { curr_player : int;
    rolled : bool;
    num_players : int;
    elim_players : int list;
    p_pos : (int*int) list; 
    p_mon : (int*int) list;
    p_prop : (int*int list) list;
    t_owner : (int*int) list;
    t_houses : (int*int) list; 
    t_mortgages : (int*bool) list;
    p_houses : (int*int) list; 
    p_hotels : (int*int) list; 
    getoutcard: (int*bool) list; 
    p_jail : (int*bool) list;
    p_turns : (int*int) list } 

(** [make_int_map tile num acc] is a helper function used to make an
    int*int association list in init_state. *)
let rec make_int_map (prop:int) (num:int) (acc:(int*int) list) : (int*int) list =
  if num = 0 then acc else make_int_map prop (num-1) ((num, prop)::acc)

(** [make_prop_map num acc] is a helper function used to initialize
    the player to property association list in init_state. *)
let rec make_prop_map (num:int) (acc:(int*int list) list) : (int*int list) list = 
  if num = 0 then acc else make_prop_map (num-1) ((num, [])::acc)

(** [make_tile_map game num acc] is a helper function used in init_state
    to initialize the tile to owner association list t_owner. *)
let rec make_tile_map game num acc = 
  if num = -1 then acc else
    let owner = tile_owner game num in 
    make_tile_map game (num-1) ((num, owner)::acc)

(* [make_mortgage_map game num acc] is a helper function used in init_state
    to initialize the tile to mortgage status association list t_mortgages. *)
let rec make_mortgage_map num acc = 
  if num = 0 then acc else make_mortgage_map (num-1) ((num,false)::acc)

(** [make_house_map tile acc] is a helper function used to initialize
    the tile to houses map in init_state. *)
let rec make_house_map tile acc = 
  if tile = -1 then acc else make_house_map (tile-1) ((tile, 0)::acc)

(** [make_bool_map num acc] is a helper function used to initialize
    the bool to players map in init_state. *)
let rec make_bool_map num acc = 
  if num = 0 then acc else make_bool_map (num-1) ((num,false)::acc)

(** [init_state game] returns the initial state of the game specified in 
    monopoly.t game *)
let init_state (game:Monopoly.t) (num:int): t = 
  {curr_player = 1;
   rolled = false;
   num_players = num;
   elim_players = [];
   p_pos = make_int_map (start_tile game) num [];
   p_mon = make_int_map (start_money game) num [];
   p_prop = make_prop_map num [];
   t_owner = make_tile_map game 39 [];
   t_houses = make_house_map 39 [];
   t_mortgages = make_mortgage_map 39 [];
   p_houses = make_int_map 0 num [];
   p_hotels = make_int_map 0 num [];
   getoutcard = make_bool_map num []; 
   p_jail = make_bool_map num []; 
   p_turns = make_int_map 0 num [] } 

(** [curr_player st] returns the current player in state [st]. *)
let curr_player st = st.curr_player

(** [get_rolled st] returns true if the current player has rolled
    and false otherwise in state [st]. *)
let get_rolled st = st.rolled 

(** [num_players st] returns the number of players initally in the state [st]*)
let num_players st = st.num_players

(* [elim_player_lst st] returns the eliminated players in state st. *)
let elim_player_lst (st:t) = st.elim_players 

(** [player_pos st] returns the player to position association list
    p_pos in state [st]. *)
let player_pos st = st.p_pos

(** [get_properties id st] returns the list of properties owned by player [id]
    in state [st]. *)
let get_properties id st = List.assoc id st.p_prop

(** [get_pocket id st] returns the amount of money owned by player [id]
    in state [st]. *)
let get_pocket id st = List.assoc id st.p_mon

(** [get_tile_owner id st] returns the owner of the tile with id [id] in
    game with current state [st]. *)
let get_tile_owner id st: int = List.assoc id st.t_owner 

(** [get_t_houses id st] returns the number of houses on tile with [id] in
    game with current state [st]. *)
let get_t_houses id st = List.assoc id st.t_houses 

(** [get_outcard id st] returns whether the player [id] owns a get out jail card in 
    game with current state [st]. *)
let get_outcard id st = List.assoc id st.getoutcard

(** [get_outcard id st] returns whether the player [id] is in jail in 
    game with current state [st]. *)
let get_jail_bool id st = List.assoc id st.p_jail

(** [get_p_houses id st] returns the houses associated with player [id] in
    game with current state [st]. *)
let get_p_houses id st = List.assoc id st.p_houses

(** [get_p_hotels id st] returns the hotels associated with player [id] in
    game with current state [st]. *)
let get_p_hotels id st = List.assoc id st.p_hotels 

(** [get_mortgaged_bool id st] returns whether the tile [id] is mortgaged in 
    game with current state [st]. *)
let get_mortgaged_bool id st = List.assoc id st.t_mortgages 

(** [next p1 num elist] is a helper function used to determine which
    player's turn is next. This is necessary because some player may be
    elimnated and are members of the list of eliminated players elist. *)
let rec next p1 num elist : int =
  let next_int = (p1 mod num) + 1 in
  if List.mem next_int elist then next next_int num elist else next_int

(** [move_p p d lst] updates the player to position association list 
    lst by moving player p d tiles on the board. This takes into account
    passing the start tile and going back to the beginning of the board. *)
let move_p (p:int) (d:int) (lst:(int*int) list) =
  let before = List.assoc p lst in 
  (p, (before + d) mod 40)::(List.remove_assoc p lst)

(** [print_board st c] dynamically prints the current state of the board. It 
    uses arrays to keep track of key elements such as player positions 
    and tile owners. *)
let print_board (st:t) (c : Monopoly.tile_info array) = 

  let a = 
    [|"    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    ";
      "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    ";
      "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    ";
      "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    "; "    ";|] in 

  let b = 
    [|" "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; 
      " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; 
      " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "|] in

  let d = 
    [|"          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          "; "          "; "          "; 
      "          "; "          "; "          "; "          " |] in

  let rec update_a st (lst:(int*int) list) = 
    begin match lst with 
      | [] -> ()
      | (player, tile)::t -> let before = a.(tile) in
        let s = if List.mem player st.elim_players then " "
          else string_of_int player in
        begin match player with
          | 1 -> a.(tile) <- s ^ String.sub before 1 3; update_a st t
          | 2 -> a.(tile) <- (String.sub before 0 1) ^ s ^ (String.sub before 2 2); update_a st t
          | 3 -> a.(tile) <- (String.sub before 0 2) ^ s ^ (String.sub before 3 1); update_a st t
          | 4 -> a.(tile) <- (String.sub before 0 3) ^ s; update_a st t 
          | _ -> failwith "Error in update_a. Too many players." end end in 
  update_a st (player_pos st);

  let rec update_b (lst:(int*int) list) =
    begin match lst with 
      | [] -> ()
      | (tile, owner)::t ->  
        if owner = -1 then update_b t 
        else b.(tile) <- string_of_int owner; update_b t end in
  update_b (st.t_owner); 

  let rec update_d (lst:(int*int) list) = 
    begin match lst with 
      | [] -> () 
      | (tile, houses)::t ->
        begin match houses with 
          | 0 -> d.(tile) <- "          "; update_d t 
          | 1 -> d.(tile) <- "      +   "; update_d t 
          | 2 -> d.(tile) <- "     ++   "; update_d t 
          | 3 -> d.(tile) <- "    +++   "; update_d t 
          | 4 -> d.(tile) <- "   ++++   "; update_d t 
          | 5 -> d.(tile) <- "  HOTEL   "; update_d t 
          | _ -> update_d t end end in 
  update_d (st.t_houses);

  let ptile (nf : Monopoly.tile_info) : string = 
    ANSITerminal.sprintf [Background (fst nf); Foreground Default; Bold] "%s" (snd nf) in

  print_endline ("__________________________________________________________________________________________________________________________");
  print_endline ("|   FREE   |" ^ ptile c.(21) ^ "|  CHANCE  |" ^ ptile c.(23) ^ "|" ^ ptile c.(24) ^ "|" ^ ptile c.(25) ^ "|" ^ ptile c.(26) ^ "|" ^ ptile c.(27) ^ "|" ^ ptile c.(28) ^ "|" ^ ptile c.(29) ^ "|  GO  TO  |");
  print_endline ("|          |" ^ d.(21) ^ "|" ^ d.(22) ^ "|" ^ d.(23) ^ "|" ^ d.(24) ^ "|" ^ d.(25) ^ "|" ^ d.(26) ^ "|" ^ d.(27) ^ "|" ^ d.(28) ^ "|" ^ d.(29) ^ "|" ^ d.(30) ^ "|");
  print_endline ("|   " ^ a.(20) ^ "   |   " ^ a.(21) ^ "   |   " ^ a.(22) ^ "   |   " ^ a.(23) ^ "   |   " ^ a.(24) ^ "   |   " ^ a.(25) ^ "   |   " ^ a.(26) ^ "   |   " ^ a.(27) ^ "   |   " ^ a.(28) ^ "   |   " ^ a.(29) ^ "   |   " ^ a.(30) ^ "   |");
  print_endline ("|__PRKING__|" ^ b.(21) ^ "_________|__________|" ^ b.(23) ^ "_________|" ^ b.(24) ^ "_________|" ^ b.(25) ^ "_________|" ^ b.(26) ^ "_________|" ^ b.(27) ^ "_________|" ^ b.(28) ^ "_________|" ^ b.(29) ^ "_________|___JAIL___|");
  print_endline ("|" ^ ptile c.(19) ^ "|                                                                                                  |" ^ ptile c.(31) ^ "|");
  print_endline ("|" ^ d.(19) ^ "|                                                                                                  |" ^ d.(31) ^ "|");
  print_endline ("|   " ^ a.(19) ^ "   |                                                                                                  |   " ^ a.(31) ^ "   |");
  print_endline ("|" ^ b.(19) ^ "_________|                                                                                                  |" ^ b.(31) ^ "_________|");
  print_endline ("|" ^ ptile c.(18) ^ "|                                                                                                  |" ^ ptile c.(32) ^ "|");
  print_endline ("|" ^ d.(18) ^ "|                                                                                                  |" ^ d.(32) ^ "|");
  print_endline ("|   " ^ a.(18) ^ "   |                                                                                                  |   " ^ a.(32) ^ "   |");
  print_endline ("|" ^ b.(18) ^ "_________|                                                                                                  |" ^ b.(32) ^ "_________|");
  print_endline ("| COMMUNTY |                                                                                                  | COMMUNTY |");
  print_endline ("|          |                                                                                                  |          |");
  print_endline ("|   " ^ a.(17) ^ "   |                                                                                                  |   " ^ a.(33) ^ "   |");
  print_endline ("|__CHEST___|                                                                                                  |__CHEST___|");
  print_endline ("|" ^ ptile c.(16) ^ "|                           ██████╗███████╗    ██████╗  ██╗ ██╗ ██████╗                            |" ^ ptile c.(34) ^ "|");
  print_endline ("|" ^ d.(16) ^ "|                          ██╔════╝██╔════╝    ╚════██╗███║███║██╔═████╗                           |" ^ d.(34) ^ "|");
  print_endline ("|   " ^ a.(16) ^ "   |                          ██║     ███████╗     █████╔╝╚██║╚██║██║██╔██║                           |   " ^ a.(34) ^ "   |");
  print_endline ("|" ^ b.(16) ^ "_________|                          ██║     ╚════██║     ╚═══██╗ ██║ ██║████╔╝██║                           |" ^ b.(34) ^ "_________|");
  print_endline ("|" ^ ptile c.(15) ^ "|                          ╚██████╗███████║    ██████╔╝ ██║ ██║╚██████╔╝                           |" ^ ptile c.(35) ^ "|");
  print_endline ("|" ^ d.(15) ^ "|                           ╚═════╝╚══════╝    ╚═════╝  ╚═╝ ╚═╝ ╚═════╝                            |" ^ d.(35) ^ "|");
  print_endline ("|   " ^ a.(15) ^ "   |                                                                                                  |   " ^ a.(35) ^ "   |");
  print_endline ("|" ^ b.(15) ^ "_________|              ███╗   ███╗ ██████╗ ███╗   ██╗ ██████╗ ██████╗  ██████╗ ██╗  ██╗   ██╗              |" ^ b.(35) ^ "_________|");
  print_endline ("|" ^ ptile c.(14) ^ "|              ████╗ ████║██╔═══██╗████╗  ██║██╔═══██╗██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝              |  CHANCE  |");
  print_endline ("|" ^ d.(14) ^ "|              ██╔████╔██║██║   ██║██╔██╗ ██║██║   ██║██████╔╝██║   ██║██║   ╚████╔╝               |" ^ d.(36) ^ "|");
  print_endline ("|   " ^ a.(14) ^ "   |              ██║╚██╔╝██║██║   ██║██║╚██╗██║██║   ██║██╔═══╝ ██║   ██║██║    ╚██╔╝                |   " ^ a.(36) ^ "   |");
  print_endline ("|" ^ b.(14) ^ "_________|              ██║╚██╔╝██║██║   ██║██║╚██╗██║██║   ██║██╔═══╝ ██║   ██║██║    ╚██╔╝                |__________|");
  print_endline ("|" ^ ptile c.(13) ^ "|              ██║ ╚═╝ ██║╚██████╔╝██║ ╚████║╚██████╔╝██║     ╚██████╔╝███████╗██║                 |" ^ ptile c.(37) ^ "|");
  print_endline ("|" ^ d.(13) ^ "|              ╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝ ╚═╝      ╚═════╝ ╚══════╝╚═╝                 |" ^ d.(37) ^ "|");
  print_endline ("|   " ^ a.(13) ^ "   |                                                                                                  |   " ^ a.(37) ^ "   |");
  print_endline ("|" ^ b.(13) ^ "_________|                                                                                                  |" ^ b.(37) ^ "_________|");
  print_endline ("|" ^ ptile c.(12) ^ "|                                                                                                  |  LUXURY  |");
  print_endline ("|" ^ d.(12) ^ "|                                                                                                  |" ^ d.(38) ^ "|");
  print_endline ("|   " ^ a.(12) ^ "   |                                                                                                  |   " ^ a.(38) ^ "   |");
  print_endline ("|" ^ b.(12) ^ "_________|                                                                                                  |___TAX____|");
  print_endline ("|" ^ ptile c.(11) ^ "|                                                                                                  |" ^ ptile c.(39) ^ "|");
  print_endline ("|" ^ d.(11) ^ "|                                                                                                  |" ^ d.(39) ^ "|");
  print_endline ("|   " ^ a.(11) ^ "   |                                                                                                  |   " ^ a.(39) ^ "   |");
  print_endline ("|" ^ b.(11) ^ "_________|__________________________________________________________________________________________________|" ^ b.(39) ^ "_________|");
  print_endline ("| JST |JAIL|" ^ ptile c.(9) ^ "|" ^ ptile c.(8) ^ "|  CHANCE  |" ^ ptile c.(6) ^ "|" ^ ptile c.(5) ^ "|  INCOME  |" ^ ptile c.(3) ^ "| COMMUNTY |" ^ ptile c.(1) ^ "|    GO    |");
  print_endline ("|     |____|" ^ d.(9) ^ "|" ^ d.(8) ^ "|" ^ d.(7) ^ "|" ^ d.(6) ^ "|" ^ d.(5) ^ "|" ^ d.(4) ^ "|" ^ d.(3) ^ "|" ^ d.(2) ^ "|" ^ d.(1) ^ "|" ^ d.(0) ^ "|");
  print_endline ("|   " ^ a.(10) ^ "   |   " ^ a.(9) ^ "   |   " ^ a.(8) ^ "   |   " ^ a.(7) ^ "   |   "^ a.(6) ^ "   |   " ^ a.(5) ^ "   |   " ^ a.(4) ^ "   |   " ^ a.(3) ^ "   |   " ^ a.(2) ^ "   |   " ^ a.(1) ^ "   |   " ^ a.(0) ^ "   |");
  print_endline ("|_VISITING_|" ^ b.(9) ^ "_________|" ^ b.(8) ^ "_________|__________|" ^ b.(6) ^ "_________|" ^ b.(5) ^ "_________|___TAX____|" ^ b.(3) ^ "_________|__CHEST___|" ^ b.(1) ^ "_________|__________|");
  print_endline ""; ()


(** [next_turn game st] advances the state of the game to the next
    player's turn. *)
let next_turn (st:t) (game:Monopoly.t) : t = 
  let next_player = next st.curr_player st.num_players st.elim_players in  
  if List.length st.elim_players <> st.num_players -1 then begin 
    let next_state = 
      {curr_player = next_player;
       rolled = false;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = st.p_pos;
       p_mon = st.p_mon;
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses;
       p_hotels = st.p_hotels;
       getoutcard = st.getoutcard;  
       p_jail = st.p_jail;
       p_turns = st.p_turns
      } in 
    print_board next_state (get_tile_info game); 
    print_endline("It is now player " ^ string_of_int next_player ^ "'s turn.");
    print_endline("Type \"roll\" to start your turn!\n"); 
    print_string("> "); next_state end
  else begin
    print_endline ("Player " ^ string_of_int next_player ^ ", you win!\n"); 
    exit 0 end

(* ------------------- helper functions to change state --------------- *)

(** [exchange amt f t lst] moves amt dollars from the pocket of player f
    to the pocket of player t by storing the new amounts in player to 
    money association list lst. Helper function for pay_rent. *)
let exchange (amt:int) (f:int) (t:int) lst =
  let from_before = List.assoc f lst in 
  let to_before = List.assoc t lst in 
  (f, from_before - amt)::(t, to_before + amt)::
  (List.remove_assoc f (List.remove_assoc t lst))

(** [remove_owners new_owner tlist tmap] updates tmap by changing the
    owner of every tile in tlist to new_owner. This is a helper function used
    when a player is eliminated from the game and all properties go
    back to player 0 *)
let rec remove_owners new_owner tlist tmap = 
  match tlist with 
  | [] -> tmap 
  | h::t -> remove_owners new_owner t ((h, new_owner)::List.remove_assoc h tmap)

(** [update_tile_owners p owner lst1 lst2] is a helper function used when players
    are eleminated to change the owner of each tile previously owned by
    player p to 0. This function calls remove_owners and is a helper
    function for pay_rent. *)
let rec update_tile_owners p new_owner lst1 lst2 = 
  match lst1 with 
  | [] -> lst2 
  | h::t -> if fst h = p then (remove_owners new_owner (snd h) lst2)
    else update_tile_owners p new_owner t lst2

(** [update_tile_owners p_tile_list t_list] is a helper function used when
    players are eliminated. It removes houses from tiles owned by the
    eliminated player. *)
let rec update_tile_houses p_tile_list t_list new_owner=
  match p_tile_list with  
  | [] -> t_list 
  | h::t -> update_tile_houses t ((h, new_owner)::(List.remove_assoc h t_list)) 
              new_owner 

(** [update_tile_mortgages p_tile_list t_list] is a helper function used when
    players are eliminated. It removes mortgages from tiles owned by the
    eliminated player. *)
let rec update_tile_mortgages p_tile_list t_list =
  match p_tile_list with 
  | [] -> t_list 
  | h::t -> update_tile_mortgages t ((h, false)::(List.remove_assoc h t_list))

(** [get_n lst n] is a helper function used to get the nth element of 
    list lst. It is used in pay_rent when finding the correct rent
    to charge based on the number of houses on a tile. *)
let rec get_n lst n =
  match lst with 
  | [] -> failwith "error in function get_n"
  | h::t -> if n = 1 then h else get_n t (n-1)

(** [num_rr p game lst] is a helper function used to find the number 
    of railroad properties owned by player p in game game with 
    tile to owner association list lst. It is called in pay_rent
    when the current tile has function railroad. *)
let rec num_rr p game lst = 
  match lst with 
  | [] -> 0 
  | (tile, owner)::t -> 
    if tile_func game tile = Railroad && owner = p 
    then 1 + num_rr p game t 
    else num_rr p game t

(** [num_util p game lst] is a helper function used to find the number 
    of utility properties owned by player p in game game with
    tile to owner association list lst. It is called in pay_rent when
    then current tile has function utilities. *)
let rec num_util p game lst = 
  match lst with 
  | [] -> 0
  | (tile, owner)::t ->
    if tile_func game tile = Utilities && owner = p
    then 1 + num_util p game t 
    else num_util p game t

(** [change_money amt p lst] adds amt dollars to player p's pocket using
    player to money association lst. Helper function used in buy. *)
let change_money amt p lst = 
  let before = List.assoc p lst in 
  (p, before + amt)::(List.remove_assoc p lst)

(** [add_prop id p lst] is a helper function used in buy that adds the
    tile with tile id to player p's list of tiles. *)
let add_prop id p lst =
  let before = List.assoc p lst in
  (p, id::before)::(List.remove_assoc p lst)

(** [elim_p p_lst elim_lst acc] is a helper function to filter [p_lst] 
    based on [elim_lst] and returns [acc].*)
let rec elim_p p_lst elim_lst acc = 
  match p_lst with 
  | [] -> acc
  | h::t -> if List.mem h elim_lst then elim_p t elim_lst acc
    else elim_p t elim_lst (h::acc) 

(** [exchange_prop prop f t lst] eliminates properties [prop] from player [f] 
    and add these properties to player [t] and adds these two players back to
    the association list of player to properties list [lst]. *)
let exchange_prop (prop: int list) (f:int) (t:int) (lst: (int*int list) list) =
  let from_before = List.assoc f lst in 
  let to_before = List.assoc t lst in 
  let new_from_lst = elim_p prop from_before [] in 
  let new_to_lst =  prop @ to_before in
  (f, new_from_lst)::(t, new_to_lst) :: 
  (List.remove_assoc f (List.remove_assoc t lst))

(* --------------------------- eliminated state ----------------------------- *)

(** [player_eliminated st owner] is a state where the current player [player] 
    gives all his values to owner [owner] *)
let player_eliminated (st : t) (owner : int) : t =
  print_endline ("Player " ^ string_of_int (owner) ^ ", you have received Player " 
                 ^ string_of_int (st.curr_player) ^"'s values."); 
  {
    curr_player = st.curr_player;
    rolled = st.rolled;
    num_players = st.num_players;
    elim_players = st.curr_player::st.elim_players;
    p_pos = st.p_pos; 
    p_mon = exchange  
        (List.assoc st.curr_player st.p_mon) st.curr_player owner st.p_mon;
    p_prop = exchange_prop (List.assoc st.curr_player st.p_prop) st.curr_player owner st.p_prop; 
    t_owner = update_tile_owners st.curr_player owner st.p_prop st.t_owner;
    t_houses = update_tile_houses (List.assoc st.curr_player st.p_prop) st.t_houses
        owner ;
    t_mortgages = update_tile_mortgages (List.assoc st.curr_player st.p_prop) st.t_mortgages;
    p_houses = exchange 
        (List.assoc st.curr_player st.p_houses) st.curr_player owner st.p_houses;
    p_hotels = exchange 
        (List.assoc st.curr_player st.p_hotels) st.curr_player owner st.p_hotels; 
    getoutcard = (owner, (List.assoc st.curr_player st.getoutcard))::
                 (List.remove_assoc owner st.getoutcard); 
    p_jail = st.p_jail;
    p_turns = st.p_turns
  } 

(* --------------------------- can't pay  ------------------------------------*)

(** [back_to_bank st] returns a state where the player's values are returned 
    to the bank. *)
let back_to_bank (st : t) : t =
  {
    curr_player = st.curr_player;
    rolled = st.rolled;
    num_players = st.num_players;
    elim_players = st.curr_player::st.elim_players;
    p_pos = st.p_pos;
    p_mon = st.p_mon;
    p_prop = st.p_prop;
    t_owner = update_tile_owners st.curr_player 0 st.p_prop st.t_owner;
    t_houses = update_tile_houses (List.assoc st.curr_player st.p_prop) 
        st.t_houses 0;

    t_mortgages = update_tile_mortgages (List.assoc st.curr_player st.p_prop) st.t_mortgages;
    p_houses = List.remove_assoc st.curr_player st.p_houses;
    p_hotels = List.remove_assoc st.curr_player st.p_hotels; 
    p_jail = st.p_jail;
    getoutcard = (st.curr_player, false)::(List.remove_assoc st.curr_player st.getoutcard); 
    p_turns = st.p_turns
  }

(** [cant_pay st game] applies [back_to_bank st] state when the player cannot
    afford to pay. *)
let cant_pay st game = 
  print_endline ("You cannot afford to pay! Your player is eliminated!/n");
  print_string (">"); 
  next_turn (back_to_bank st) game 

(* ------------------------------- Jail ------------------------------------- *)

(** [check_gotojail st game] is the new state in which the player lands on the 
    tile "Go To Jail" in the current state [st] in game [game]. *)
let check_gotojail st game : t =
  let p_now_injail_state =
    {curr_player = st.curr_player;
     rolled = st.rolled;
     num_players = st.num_players;
     elim_players = st.elim_players;
     p_pos = move_p st.curr_player 20 st.p_pos; 
     p_mon = st.p_mon ;
     p_prop = st.p_prop;
     t_owner = st.t_owner;
     t_houses = st.t_houses;
     t_mortgages = st.t_mortgages;
     p_houses = st.p_houses;
     p_hotels = st.p_hotels;
     getoutcard = st.getoutcard;  
     p_jail = (st.curr_player,true) :: (List.remove_assoc st.curr_player st.p_jail);
     p_turns = (st.curr_player, 1) :: List.remove_assoc st.curr_player st.p_turns} in 
  next_turn p_now_injail_state game  

(** [check_gotojail st game] is the new state of the player on the tile "Jail" 
    in current state [st] in game [game]. If the player did not pass "Go To Jail" 
    then the player is just visiting jail. The player must either pay $50 or 
    use a Get Out of Jail card within 3 turns to get out of jail, 
    otherwise the player is eliminated.*)
let check_leavejail st game = 
  let jail_bool = List.assoc st.curr_player st.p_jail in
  let num_ofturns = List.assoc st.curr_player st.p_turns in 
  if (jail_bool = true) then begin

    if (num_ofturns <=2) then begin 
      print_endline ("You are currently in jail. You have " ^ string_of_int (3-num_ofturns) 
                     ^ " complete turns left.");
      print_endline ("Type \"pay jail fee\" to pay $50 and leave.");
      print_endline ("You can also use a get out of jail card to leave by typing 
      \"get out card\".");
      print_endline ("Type \"end\" to skip your turn.\n" );
      print_string ("> "); 
      {curr_player = st.curr_player;
       rolled = true;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = st.p_pos;
       p_mon = st.p_mon;
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses;
       p_hotels = st.p_hotels;
       p_jail = st.p_jail;
       getoutcard = st.getoutcard; 
       p_turns = let curr_p_turns = List.assoc st.curr_player st.p_turns in
         ((st.curr_player,curr_p_turns+1) :: List.remove_assoc st.curr_player st.p_turns)}
    end

    else if (num_ofturns = 3) then begin
      print_endline ("This is your final turn in jail. You will lose if you skip your turn.");
      print_endline ("Type \"pay jail fee\" to pay the $50 fine in order to leave.");
      print_endline ("Or use a get out of jail card to leave by typing \"get out card\".");
      print_endline ("Type \"end\" to skip your turn.\n");
      print_string ("> ");
      {curr_player = st.curr_player;
       rolled = true;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = st.p_pos;
       p_mon = st.p_mon;
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses;
       p_hotels = st.p_hotels;
       p_jail = st.p_jail;
       getoutcard = st.getoutcard; 
       p_turns = let curr_p_turns = List.assoc st.curr_player st.p_turns in
         ((st.curr_player,curr_p_turns+1) :: List.remove_assoc st.curr_player st.p_turns); }
    end

    else begin 
      print_endline ("Player " ^ string_of_int st.curr_player ^ 
                     ", you did not get out of jail! You are eliminated from the game."); 
      next_turn (back_to_bank st) game end end

  else begin 
    print_endline("You are just visiting. There is nothing to do here.");
    print_endline("Type \"end\" to end your turn.\n");
    print_string ("> ");
    st end 

(** [pay_jail st game] is the new state when the player pays the $50 jail fee
    in current state [st] in game [game]. If the player does not have enough 
    money, then the player is in the state [cant_play st game].*)
let pay_jail st game = 
  print_endline ("You have now paid the jail fee!");
  print_endline ("Type \"end\" to end your turn. \n");
  print_string ("> ");
  if 50 > (List.assoc st.curr_player st.p_mon) then 
    cant_pay st game 
  else 
    {curr_player = st.curr_player;
     rolled = true;
     num_players = st.num_players;
     elim_players = st.elim_players;
     p_pos = st.p_pos; 
     p_mon = change_money (-50) st.curr_player st.p_mon;
     p_prop = st.p_prop;
     t_owner = st.t_owner;
     t_mortgages = st.t_mortgages;
     t_houses = st.t_houses;
     p_houses = st.p_houses;
     p_hotels = st.p_hotels;
     getoutcard = st.getoutcard;  
     p_jail = (st.curr_player,false) :: (List.remove_assoc st.curr_player st.p_jail);
     p_turns = (st.curr_player, 0) :: (List.remove_assoc st.curr_player st.p_turns)} 

(** [jail_card st game] returns a new state when the player applies the Get Out of
    Jail card in current state [st] in game [game].*)
let jail_card st game =  
  if List.assoc st.curr_player st.getoutcard = false then begin 
    print_endline ("You do not have this card. ");
    print_endline ("Pay the jail fee or type \"end\" to end your turn. \n");
    print_string ("> "); st end 
  else begin 
    print_endline ("You have used the get out card!");
    print_endline ("Type \"end\" to end your turn. \n");
    print_string ("> "); 
    {curr_player = st.curr_player;
     rolled = true;
     num_players = st.num_players;
     elim_players = st.elim_players;
     p_pos = st.p_pos; 
     p_mon = st.p_mon ;
     p_prop = st.p_prop;
     t_owner = st.t_owner;
     t_mortgages = st.t_mortgages;
     t_houses = st.t_houses;
     p_houses = st.p_houses;
     p_hotels = st.p_hotels;
     getoutcard = (st.curr_player,false) :: (List.remove_assoc st.curr_player st.getoutcard);
     p_jail = (st.curr_player,false) :: (List.remove_assoc st.curr_player st.p_jail);
     p_turns = (st.curr_player, 0) :: List.remove_assoc st.curr_player st.p_turns} 
  end

(* -------------------------end for jail ---------------------------------- *)

(* ----------------------- cards for monopoly ------------------------------ *)

(** [apply card st game card] is the new state in which the cards are applied
    in current state [st] in game [game]. For the card in which the player 
    pays, if the player cannot pay, then the player is eliminated and his 
    values return to the bank.*)
let apply_card (st:t) (game:Monopoly.t) (card : card): t =
  print_endline ("Type \"end\" to end your turn, or type \"help\" for other commands.");
  let curr_p_houses = List.assoc st.curr_player st.p_houses in 
  let curr_p_hotels = List.assoc st.curr_player st.p_hotels in 
  let prop_charge = ((card_modi card)* curr_p_houses)+ 
                    ((card_modi card) * 4 * curr_p_hotels)  in 
  match card_action card with 
  | Money -> if (card_modi card) > List.assoc st.curr_player st.p_mon then 
      cant_pay st game else 
      let next_state = 
        {curr_player = st.curr_player;
         rolled = st.rolled;
         num_players = st.num_players;
         elim_players = st.elim_players;
         p_pos = st.p_pos;
         p_mon = change_money (card_modi card) st.curr_player st.p_mon;
         p_prop = st.p_prop;
         t_owner = st.t_owner;
         t_houses = st.t_houses;
         t_mortgages = st.t_mortgages;
         p_houses = st.p_houses; 
         p_hotels = st.p_hotels; 
         p_jail = st.p_jail;
         getoutcard = st.getoutcard; 
         p_turns = st.p_turns
        } 
      in 
      print_board next_state (get_tile_info game); 
      print_endline (card_title card ^ "\n"); print_string("> "); next_state 
  | GoToTile  -> 
    let next_state = 
      {curr_player = st.curr_player;
       rolled = st.rolled;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = (st.curr_player,(card_modi card))::
               (List.remove_assoc st.curr_player st.p_pos); 
       p_mon = if List.assoc st.curr_player st.p_pos > (card_modi card) then 
           change_money (200) st.curr_player st.p_mon
         else st.p_mon ;
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses; 
       p_hotels = st.p_hotels; 
       p_jail = st.p_jail;
       getoutcard = st.getoutcard; 
       p_turns = st.p_turns
      } 
    in 
    print_board next_state (get_tile_info game); 
    print_endline (card_title card ^ "\n"); print_string("> "); next_state 
  | MoveSpaces -> 
    let next_state = 
      {curr_player = st.curr_player;
       rolled = st.rolled;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = move_p st.curr_player (card_modi card) st.p_pos;
       p_mon =  if (List.assoc st.curr_player st.p_pos) + (card_modi card) >= 40 
         then change_money (200) st.curr_player st.p_mon else
           st.p_mon; 
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       p_houses = st.p_houses; 
       t_mortgages = st.t_mortgages;
       p_hotels = st.p_hotels; 
       p_jail = st.p_jail;
       getoutcard = st.getoutcard; 
       p_turns = st.p_turns
      } in 
    print_board next_state (get_tile_info game); 
    print_endline ((card_title card) ^ "\n"); print_string("> "); next_state 
  | GetOutJail -> 
    let next_state = 
      {curr_player = st.curr_player;
       rolled = st.rolled;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = st.p_pos;
       p_mon = st.p_mon; 
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses; 
       p_hotels = st.p_hotels; 
       p_jail = st.p_jail;
       getoutcard = (st.curr_player,true):: (List.remove_assoc st.curr_player st.getoutcard); 
       p_turns = st.p_turns
      } in
    print_endline (card_title card); 
    print_endline ("You may keep this card and apply when needed by 
    typing \"get out\"."); print_string("> "); next_state
  | GoJail -> 
    let next_state = 
      {curr_player = st.curr_player;
       rolled = st.rolled;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = (st.curr_player, 10)::(List.remove_assoc st.curr_player st.p_pos);
       p_mon =  st.p_mon; 
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses; 
       p_hotels = st.p_hotels; 
       p_jail = (st.curr_player,true)::(List.remove_assoc st.curr_player st.p_jail);
       getoutcard = st.getoutcard;
       p_turns = st.p_turns
      } in 
    print_board next_state (get_tile_info game); 
    print_endline (card_title card ^ "\n"); print_string("> "); next_state 
  | PropertyCharge -> 
    let next_state = 
      {curr_player = st.curr_player;
       rolled = st.rolled;
       num_players = st.num_players;
       elim_players = st.elim_players;
       p_pos = st.p_pos; 
       p_mon = change_money prop_charge st.curr_player st.p_mon; 
       p_prop = st.p_prop;
       t_owner = st.t_owner;
       t_houses = st.t_houses;
       t_mortgages = st.t_mortgages;
       p_houses = st.p_houses; 
       p_hotels = st.p_hotels; 
       p_jail =  st.p_jail;
       getoutcard = st.getoutcard;
       p_turns = st.p_turns 
      } in 
    print_board next_state (get_tile_info game); 
    print_endline (card_title card ^ "\n"); print_string("> "); next_state 

(** [play_chance_card st game] is the new state when a random card from chance
    is played in current state [st] in game [game]. *)
let play_chance_card st game= 
  apply_card st game (get_chance_card game (1 + Random.int 14)) 

(** [play_comm_card st game] is the new state when a random card from community
    chest is played in current state [st] in game [game]. *)
let play_comm_card st game = 
  apply_card st game (get_comm_card game (1 + Random.int 14)) 

(** [choose_chance st game] is the new state when a choosen card from chance
    is played in current state [st] in game [game]. *)
let choose_chance st game int = 
  apply_card st game (get_chance_card game int) 

(** [choose_comm st game] is the new state when a choosen card from community
    chest is played in current state [st] in game [game]. *)
let choose_comm st game int = 
  apply_card st game (get_comm_card game int) 

(* ------------------------ end for cards ---------------------------------- *)

(* ----------------------- income and luxury tax --------------------------- *)

(**[pay_tax game st amt] is the new state in which the player pays [amt] when 
   landing on income or luxury tax.*)
let pay_tax game st amt= 
  if amt > List.assoc st.curr_player st.p_mon then 
    cant_pay st game 
  else 
    let next_state = 
      {
        curr_player = st.curr_player;
        rolled = st.rolled;
        num_players = st.num_players;
        elim_players = st.elim_players;
        p_pos = st.p_pos;
        p_mon = change_money (0 - amt) st.curr_player st.p_mon;
        p_prop = st.p_prop;
        t_owner = st.t_owner;
        t_houses = st.t_houses;
        t_mortgages = st.t_mortgages;
        p_houses = st.p_houses; 
        p_hotels = st.p_hotels; 
        p_jail = st.p_jail;
        getoutcard = st.getoutcard;
        p_turns = st.p_turns;
      } in 
    print_endline ("You paid $" ^ string_of_int amt ^ " to the bank.\n"); 
    print_string (">"); next_state 

(* -------------------- end for income / luxury tax --------------------  *)

(** [pay_rent st game] transfers an amount of money equal to the rent of the
    tile that the current player is on to the owner of the tile if the tile
    is owned by a player that is not the current player. *)
let pay_rent (st:t) (game:Monopoly.t) (roll:int): t =
  let tile = List.assoc st.curr_player st.p_pos in 
  let owner = List.assoc tile st.t_owner in 
  if owner = -1 then begin
    match (tile_func game tile) with 
    | Chance -> 
      print_endline ("You got a CHANCE CARD!"); 
      (*   print_endline ("Type \"end\" to end your turn, or type \"help\" for other commands."); *)
      play_chance_card st game
    | Chest -> 
      print_endline ("You got a COMMUNITY CHEST CARD!"); 
      (*   print_endline ("Type \"end\" to end your turn, or type \"help\" for other commands."); *)
      play_comm_card st game  
    | Income -> pay_tax game st 200
    | Luxury -> pay_tax game st 100 
    | FreeParking -> 
      print_endline ("There is nothing to do here. Type \"end\" to end your turn.");
      print_string ("> "); 
      st
    | GoToJail -> print_endline ("You will be going to Jail!");
      check_gotojail st game    
    | Jail -> check_leavejail st game 
    | _ -> print_endline ("This is a special tile. You cannot buy it. 
    Type \"roll\" if you have not rolled, otherwise type \"end\" to end your turn." ); 
      print_string (">"); st end 
  else if st.curr_player = owner then begin
    print_endline ("You own this tile, so you do not have to pay rent."); 
    print_endline ("Type \"end\" to end your turn."); print_string("> "); st end 
  else if owner = 0 then begin
    print_endline("This property is not currently owned! You may purchase this property for $" 
                  ^ string_of_int (tile_price game tile) ^ "! Type \"buy\"!"); 
    print_endline("Type \"help\" for instructions."); 
    print_endline("If you do not wish to buy, type \"end\" to end your turn.\n"); 
    print_string("> "); st end 
  else if (get_mortgaged_bool tile st) then begin
    print_endline("This property is currently mortgaged and does not collect rent.");
    print_string("> "); st end 
  else
    let pay rent =
      if (rent) > List.assoc st.curr_player st.p_mon then begin
        print_endline "You cannot afford this rent! Your player is eliminated!";
        next_turn (player_eliminated st owner) game end
      else begin
        print_endline ("You paid $" ^ string_of_int (rent) ^ " to player " ^ 
                       string_of_int owner ^ ".");
        print_endline ("Type \"end\" to end your turn.\n"); 
        print_string("> ");
        {curr_player = st.curr_player;
         rolled = st.rolled;
         num_players = st.num_players;
         elim_players = st.elim_players;
         p_pos = st.p_pos;
         p_mon = exchange (rent) st.curr_player owner st.p_mon;
         p_prop = st.p_prop;
         t_owner = st.t_owner;
         t_houses = st.t_houses;
         t_mortgages = st.t_mortgages;
         p_houses = st.p_houses; 
         p_hotels = st.p_hotels; 
         p_jail = st.p_jail;
         getoutcard = st.getoutcard;
         p_turns = st.p_turns
        } end
    in let func = tile_func game tile in 
    match func with 
    | Property x -> 
      let rent = begin
        match List.assoc tile st.t_houses with 
        | 0 -> tile_rent game tile 
        | n -> get_n (tile_multirent game tile) n end
      in pay rent 
    | Railroad -> let num = num_rr owner game st.t_owner in 
      let rent = (25 * num) in pay rent
    | Utilities -> let num = num_util owner game st.t_owner in 
      let rent = ((num * 5) * roll) in pay rent
    | _ -> st

(** [roll st game] advances the current player a random number of tiles
    between 2 and 12. *)
let roll (st:t) (game:Monopoly.t): t = 
  let jail_bool = List.assoc st.curr_player st.p_jail in
  if (List.assoc st.curr_player st.p_mon) <= 0 then begin 
    print_endline ("You died!"); 
    next_turn (back_to_bank st) game end else begin 
    if st.rolled then begin
      print_endline("You have already rolled! Type \"end\" to end your turn.\n"); 
      print_string("> "); st end
    else if st.rolled = false && jail_bool = true then 
      check_leavejail st game 
    else begin
      Random.self_init ();
      let dice1 = 1 + Random.int 5 in
      let dice2 = 1 + Random.int 5 in
      let both_die = dice1 + dice2 in
      let tile = ((List.assoc st.curr_player st.p_pos) + both_die) mod 40 in 
      let next_state =
        {curr_player = st.curr_player;
         rolled = true;
         num_players = st.num_players;
         elim_players = st.elim_players;
         p_pos = move_p st.curr_player both_die st.p_pos;
         p_mon = 
           if (List.assoc st.curr_player st.p_pos) + both_die >= 40 then
             (st.curr_player, 200 + (List.assoc st.curr_player st.p_mon))::
             (List.remove_assoc st.curr_player st.p_mon) else
             st.p_mon;
         p_prop = st.p_prop;
         t_owner = st.t_owner;
         t_houses = st.t_houses;
         t_mortgages = st.t_mortgages;
         p_houses = st.p_houses; 
         p_hotels = st.p_hotels; 
         p_jail = st.p_jail;
         getoutcard = st.getoutcard;
         p_turns = st.p_turns
        } in 
      print_board next_state (get_tile_info game);   
      print_endline ("You rolled a " ^ string_of_int both_die ^ " and now your character is on " ^ 
                     tile_name game tile ^ "."); 
      print_endline ("To learn more about this tile, type \"tile\".\n");
      pay_rent next_state game both_die end end 

(** [rolln st game n ] advances the current player n tiles in game game with
    current state st. *)
let rolln (st:t) (game:Monopoly.t) (n:int) : t =
  let tile = ((List.assoc st.curr_player st.p_pos) + n) mod 40 in 
  let next_state =
    {curr_player = st.curr_player;
     rolled = true;
     num_players = st.num_players;
     elim_players = st.elim_players;
     p_pos = move_p st.curr_player n st.p_pos;
     p_mon = 
       if (List.assoc st.curr_player st.p_pos) + n >= 40 then
         (st.curr_player, 200 + (List.assoc st.curr_player st.p_mon))::
         (List.remove_assoc st.curr_player st.p_mon) else
         st.p_mon;
     p_prop = st.p_prop;
     t_owner = st.t_owner;
     t_houses = st.t_houses;
     t_mortgages = st.t_mortgages;
     p_houses = st.p_houses; 
     p_hotels = st.p_hotels; 
     p_jail = st.p_jail;
     getoutcard = st.getoutcard;
     p_turns = st.p_turns
    } in 
  print_board next_state (get_tile_info game);   
  print_endline ("You rolled a " ^ string_of_int n ^ " and now your character is on " 
                 ^ tile_name game tile ^ "."); 
  print_endline ("To learn more about this tile, type \"tile\".\n"); next_state 


(** [buy st game] adds the tile that the current player is on the player's
    list of properties and changes the tile's owner to the current player,
    if it is possible for the current player to buy the current tile
    (i.e., the player has enough money, and the tile is a tile that
    can be bought). *)
let buy (st:t) (game:Monopoly.t): t =
  if not st.rolled then begin
    print_endline("You must roll to start your turn. Type \"roll\".\n"); 
    print_string (">"); 
    st end 
  else
    let tile = List.assoc st.curr_player st.p_pos in 
    let owner = List.assoc tile st.t_owner in 
    let cost = tile_price game tile in 
    let pocket = List.assoc st.curr_player st.p_mon in
    if owner = -1 then begin
      print_endline("You cannot buy this tile.\n"); print_string ("> "); st end
    else if owner = 0 then 
      if not (cost > pocket) then begin
        let next_state = 
          {curr_player = st.curr_player;
           rolled = st.rolled;
           num_players = st.num_players;
           elim_players = st.elim_players;
           p_pos = st.p_pos;
           p_mon = change_money (0 - cost) st.curr_player st.p_mon;
           p_prop = add_prop tile st.curr_player st.p_prop;
           t_owner = (tile, st.curr_player)::(List.remove_assoc tile st.t_owner);
           t_houses = st.t_houses;
           t_mortgages = st.t_mortgages;
           p_houses = st.p_houses; 
           p_hotels = st.p_hotels; 
           p_jail = st.p_jail;
           getoutcard = st.getoutcard;
           p_turns = st.p_turns
          } in 
        print_board next_state (get_tile_info game);
        print_endline("You bought this property for $" ^ string_of_int cost ^ "!"); 
        print_endline ("To check your status, type \"status\"."); 
        print_endline("Type \"end\" to end your turn.\n");
        print_string("> "); next_state end
      else begin print_endline "You do not have enough money! Type \"end\" to end your turn.\n"; 
        print_string("> "); st end
    else begin print_endline ("This property is not for sale! The owner is Player " ^ string_of_int owner ^ ".");
      print_endline ("Type \"end\" to end your turn.\n"); print_string("> "); 
      st end

(** [own_set st game player s] is a helper function that determines if
    player p owns every property in the set s in game game with current
    state st. It is called when a player attempts to buy a house in
    buy_house since a player must own all properties in a set before
    buying a house. *)
let own_set (st:t) (game:Monopoly.t) (player:int) (s:ANSITerminal.color) : bool =
  let set = get_set game s in
  let rec helper set player = 
    match set with 
    | [] -> true 
    | h::t -> if List.assoc h st.t_owner = player then helper t player else false
  in helper set player 


(** [buy_house st game id] adds a house to tile id in game game with
    current state st, if the following conditions are met:
    1. The owner of tile id is the current player
    2. The current player owns all properties in the set that contains tile id
    3. Tile id is a tile on which it is possible to build houses (i.e. its
       function is Property)
    4. The current player has enough money to buy a house on tile id
    5. Tile id currently has no more than three houses on it *)
let buy_house (st:t) (game:Monopoly.t) (id:int) : t =
  let owner = List.assoc id st.t_owner in 
  match owner with 
  | i when i = st.curr_player -> 
    let func = tile_func game id in begin
      match func with 
      | Property x ->
        if own_set st game st.curr_player x then
          let cost = tile_house_cost game id in 
          let mortgage_status = List.map (fun x -> get_mortgaged_bool x st) (get_set game x) in
          if List.mem true mortgage_status then begin 
            print_endline("If a property of the same color is mortgaged then you cannot purchase houses."); 
            print_endline("Properties of this color mortgaged: " ^ 
                          List.fold_left (^) "" 
                            (List.map 
                               (fun y -> if (get_mortgaged_bool y st) then (tile_name game y) ^ " " else "") 
                               (get_set game x))); 
            print_string("> "); st end
          else if cost <= List.assoc st.curr_player st.p_mon then
            let t_num = List.assoc id st.t_houses in
            let p_num = List.assoc st.curr_player st.p_houses in 
            if t_num > 3 then begin
              print_endline ("You cannot purchase any more houses on this property."); 
              print_string ("> "); st end
            else let next_state = 
                   {
                     curr_player = st.curr_player;
                     rolled = st.rolled;
                     num_players = st.num_players;
                     elim_players = st.elim_players;
                     p_pos = st.p_pos;
                     p_mon = change_money (0 - cost) st.curr_player st.p_mon;
                     p_prop = st.p_prop;
                     t_owner = st.t_owner;
                     t_houses = 
                       (id, t_num + 1)::(List.remove_assoc id st.t_houses);
                     t_mortgages = st.t_mortgages;
                     p_houses = 
                       (st.curr_player, p_num + 1)::(List.remove_assoc st.curr_player st.p_houses);
                     p_hotels = st.p_hotels; 
                     p_jail = st.p_jail;
                     getoutcard = st.getoutcard;
                     p_turns = st.p_turns
                   } in 
              print_board next_state (get_tile_info game);     
              print_endline("You purchased a house on " ^ tile_name game id ^ "."); 
              print_endline("To sell a house, type \"sell house\" followed by the tile shortcode.\n");
              print_string("> "); next_state
          else begin print_endline("You do not have enough money to purchase a house.\n"); 
            print_string("> "); st end
        else begin 
          print_endline("You must own all properties in this set before you can buy a house.\n"); 
          print_string("> "); st end
      | _ -> print_endline("You cannot build houses on this property.\n"); 
        print_string("> "); st end
  | _ -> print_endline("You do not own this property so you cannot build houses on it.\n"); 
    print_string("> "); st 


(** [sell_house st game id] removes a house from tile id and adds the
    cost of the house to the current player's account if the following
    conditions are met:
    1. The owner of tile id is the current player
    2. The tile has at least one house on it *)
let sell_house (st:t) (game:Monopoly.t) (id:int) : t = 
  let owner = List.assoc id st.t_owner in 
  let t_num = List.assoc id st.t_houses in
  let p_num = List.assoc st.curr_player st.p_houses in
  if st.curr_player = owner then 
    if t_num = 0 || t_num = 5 then begin
      print_endline ("There are no houses on this property.\n");
      print_string ("> "); st end
    else let cost = tile_house_cost game id in 
      let next_state = 
        {
          curr_player = st.curr_player;
          rolled = st.rolled;
          num_players = st.num_players;
          elim_players = st.elim_players;
          p_pos = st.p_pos;
          p_mon = change_money (cost) st.curr_player st.p_mon;
          p_prop = st.p_prop;
          t_owner = st.t_owner;
          t_houses = (id, t_num - 1)::(List.remove_assoc id st.t_houses);
          t_mortgages = st.t_mortgages;
          p_houses = (st.curr_player, p_num - 1)::(List.remove_assoc st.curr_player st.p_houses);
          p_hotels = st.p_hotels;
          p_jail = st.p_jail;
          getoutcard = st.getoutcard;
          p_turns = st.p_turns
        } in begin
        print_board next_state (get_tile_info game);
        print_endline("You sold a house on " ^ tile_name game id ^ " for $" ^ string_of_int cost ^ ".\n");
        print_string("> "); next_state end
  else begin print_endline("You do not own that property.\n");
    print_string("> "); st end


(** [sell hotel st game id] removes a hotel from tile id in game game with
    state st and adds the cost to the current player's pocket if the owner of 
    the tile is the current player and the tile has a hotel on it. After
    selling the hotel, the tile will have four houses. *)
let sell_hotel (st:t) (game:Monopoly.t) (id:int) : t = 
  let owner = List.assoc id st.t_owner in 
  let t_num = List.assoc id st.t_houses in
  let p_house = List.assoc st.curr_player st.p_houses in
  let p_hotel = List.assoc st.curr_player st.p_hotels in
  if st.curr_player = owner then 
    if t_num <> 5 then begin
      print_endline ("There is no hotel on this property.\n");
      print_string ("> "); st end
    else let cost = tile_hotel_cost game id in 
      let next_state = 
        {
          curr_player = st.curr_player;
          rolled = st.rolled;
          num_players = st.num_players;
          elim_players = st.elim_players;
          p_pos = st.p_pos;
          p_mon = change_money (cost) st.curr_player st.p_mon;
          p_prop = st.p_prop;
          t_owner = st.t_owner;
          t_houses = (id, t_num - 1)::(List.remove_assoc id st.t_houses);
          t_mortgages = st.t_mortgages;
          p_houses = (st.curr_player, p_house + 4)::(List.remove_assoc st.curr_player st.p_houses);
          p_hotels = (st.curr_player, p_hotel - 1)::(List.remove_assoc st.curr_player st.p_hotels);
          p_jail = st.p_jail;
          getoutcard = st.getoutcard;
          p_turns = st.p_turns
        } in begin
        print_board next_state (get_tile_info game);
        print_endline("You sold a hotel on " ^ tile_name game id ^ " for $" ^ string_of_int cost ^ ".\n");
        print_string("> "); next_state end
  else begin print_endline("You do not own that property.\n");
    print_string("> "); st end


(** [buy_hotel st game id] adds a hotel to and removes four houses from 
    tile id in game game with current state st, if the following 
    conditions are met:
    1. The owner of tile id is the current player
    2. The current player owns all properties in the set that contains tile id
    3. Tile id is a tile on which it is possible to build hotels (i.e. its
       function is Property)
    4. The current player has enough money to buy a hotel on tile id
    5. Tile id currently has exactly four houses on it. *)
let buy_hotel (st:t) (game:Monopoly.t) (id:int) : t =
  let owner = List.assoc id st.t_owner in 
  match owner with 
  | i when i = st.curr_player -> 
    let func = tile_func game id in begin
      match func with 
      | Property x -> 
        if own_set st game st.curr_player x then
          let cost = tile_hotel_cost game id in 
          if cost <= List.assoc st.curr_player st.p_mon then
            let num = List.assoc id st.t_houses in
            let p_house = List.assoc st.curr_player st.p_houses in 
            let p_hotel = List.assoc st.curr_player st.p_hotels in
            if num < 4 then begin
              print_endline ("You cannot purchase a hotel on this property. You must first build four houses.\n"); 
              print_string ("> "); st end
            else if num > 4 then begin
              print_endline ("This property already has a hotel.\n"); 
              print_string ("> "); st end 
            else let next_state = 
                   {
                     curr_player = st.curr_player;
                     rolled = st.rolled;
                     num_players = st.num_players;
                     elim_players = st.elim_players;
                     p_pos = st.p_pos;
                     p_mon = change_money (0 - cost) st.curr_player st.p_mon;
                     p_prop = st.p_prop;
                     t_owner = st.t_owner;
                     t_houses = (id, num + 1)::(List.remove_assoc id st.t_houses);
                     t_mortgages = st.t_mortgages;
                     p_houses = (st.curr_player, p_house - 4)::(List.remove_assoc st.curr_player st.p_houses); 
                     p_hotels = (st.curr_player, p_hotel + 1)::(List.remove_assoc st.curr_player st.p_hotels);
                     p_jail = st.p_jail;
                     getoutcard = st.getoutcard;
                     p_turns = st.p_turns
                   } in 
              print_board next_state (get_tile_info game);     
              print_endline("You purchased a hotel on " ^ tile_name game id ^ ".");
              print_endline("To sell a hotel, type \"sell hotel\" followed by the tile shortcode.\n"); 
              print_string ("> "); next_state
          else begin print_endline("You do not have enough money to purchase a hotel.\n"); 
            print_string ("> "); st end
        else begin 
          print_endline("You must own all properties in this set before you can build houses or hotels on it.\n"); 
          print_string("> "); st end
      | _ -> print_endline("You cannot build houses or hotels on this property.\n"); 
        print_string("> "); st end
  | _ -> print_endline("You do not own this property so you cannot build houses or hotels on it.\n"); 
    print_string("> "); st 



let mortgage_value (st:t) (game:Monopoly.t) (id:int) : int =
  let num = List.assoc id st.t_houses in
  if num <= 4 then
    num*(tile_house_cost game id) + (tile_price game id)/2
  else
    (tile_hotel_cost game id) + (tile_price game id)/2

let rec update_p_houses (st:t) p_tile_list p_list =
  match p_tile_list with 
  | [] -> p_list 
  | h::t -> begin
      let num = List.assoc h st.t_houses in
      if num <= 4 then
        let al = (st.curr_player, (List.assoc st.curr_player p_list) - num) in
        update_p_houses st t (al::(List.remove_assoc st.curr_player p_list))
      else
        let al = (st.curr_player, (List.assoc st.curr_player p_list) - 4) in
        update_p_houses st t (al::(List.remove_assoc st.curr_player p_list))
    end

let rec update_p_hotels (st:t) p_tile_list p_list =
  match p_tile_list with 
  | [] -> p_list 
  | h::t -> begin
      let num = List.assoc h st.t_houses in
      if num <= 4 then
        let al = (st.curr_player, (List.assoc st.curr_player st.p_hotels)) in
        update_p_houses st t (al::(List.remove_assoc st.curr_player p_list))
      else
        let al = (st.curr_player, (List.assoc st.curr_player st.p_hotels) - (num - 4)) in
        update_p_houses st t (al::(List.remove_assoc st.curr_player p_list))
    end

(** [mortgage st game id] mortgages tile [id], selling all properties from
    tiles of the same for half their value and returning half the value of the, 
    property. Hotels and houses can no longer be bought all mortgages lifted. *)
let mortgage (st:t) (game:Monopoly.t) (id:int) : t = 
  let owner = List.assoc id st.t_owner in 
  let mort_amt = (tile_price game id)/2 in
  match owner with 
  | i when i = st.curr_player -> 
    let func = tile_func game id in begin
      match func with 
      | Property x ->
        if (get_mortgaged_bool id st) then begin
          print_endline("You've already mortgaged this tile!\n"); 
          print_endline("With 10% interest, unmortgaging will cost $" ^ string_of_int (mort_amt + mort_amt/10) ^ ".");
          print_endline("To mortgage a property, type \"unmortgage\" followed by the tile shortcode.\n"); 
          print_string ("> "); st end
        else if not (own_set st game st.curr_player x) then begin
          let next_state = 
            {
              curr_player = st.curr_player;
              rolled = st.rolled;
              num_players = st.num_players;
              elim_players = st.elim_players;
              p_pos = st.p_pos;
              p_mon = change_money mort_amt st.curr_player st.p_mon;
              p_prop = st.p_prop;
              t_owner = st.t_owner;
              t_mortgages = (id,true)::(List.remove_assoc id st.t_mortgages);
              t_houses = st.t_houses;
              p_houses = st.p_houses;
              p_hotels = st.p_hotels;
              p_jail = st.p_jail;
              getoutcard = st.getoutcard;
              p_turns = st.p_turns
            } in
          print_board next_state (get_tile_info game);     
          print_endline("You mortgaged " ^ tile_name game id ^ " and received $" ^ string_of_int mort_amt ^ ".\n");
          print_endline("With 10% interest, unmortgaging will cost $" ^ string_of_int (mort_amt + mort_amt/10) ^ ".");
          print_endline("To unmortgage a property, type \"unmortgage\" followed by the tile shortcode.\n"); 
          print_string ("> "); next_state end
        else begin
          let set = get_set game x in 
          let total_mort_amt = List.fold_left (+) 0 (List.map (mortgage_value st game) set) in
          let next_state = 
            {
              curr_player = st.curr_player;
              rolled = st.rolled;
              num_players = st.num_players;
              elim_players = st.elim_players;
              p_pos = st.p_pos;
              p_mon = change_money total_mort_amt st.curr_player st.p_mon;
              p_prop = st.p_prop;
              t_owner = st.t_owner;
              t_mortgages = (id, true)::(List.remove_assoc id st.t_mortgages);
              t_houses = update_tile_houses set st.t_houses 0;
              p_houses = update_p_houses st set st.t_houses;
              p_hotels = update_p_hotels st set st.t_houses;
              p_jail = st.p_jail;
              getoutcard = st.getoutcard;
              p_turns = st.p_turns
            } in
          print_board next_state (get_tile_info game);     
          print_endline("You mortgaged " ^ tile_name game id ^ " and received $" ^ string_of_int ((tile_price game id)/2) ^ ".");
          print_endline("In addition, mortgaging requires the sale of all houses and hotels on properties of the same color.");
          print_endline("As a result, in total you received $" ^ string_of_int mort_amt ^ ".\n");
          print_endline("With 10% interest, unmortgaging will cost $" ^ string_of_int (mort_amt + mort_amt/10) ^ ".");
          print_endline("To unmortgage a property, type \"unmortgage\" followed by the tile shortcode.\n"); 
          print_string ("> "); next_state end
      | _ -> print_endline("You can only mortgage properties.\n"); 
        print_string("> "); st end
  | _ -> print_endline("You do not own this property so you cannot mortgage it.\n"); 
    print_string("> "); st 

(** [unmortgage st game id] unmortgages tile [id], removing the original 
    mortgage raise and 10% interest from the players pocket and enabling
    the purchase of houses and hotels on all tiles of the same color. *)
let unmortgage (st:t) (game:Monopoly.t) (id:int) : t = 
  let owner = List.assoc id st.t_owner in 
  let cost = ((tile_price game id)/2) + ((tile_price game id)/20) in 
  match owner with 
  | i when i = st.curr_player -> 
    let func = tile_func game id in begin
      match func with 
      | Property x ->
        if not (get_mortgaged_bool id st) then begin
          print_endline("You haven't mortgaged this tile!"); 
          print_endline("To mortgage a property, type \"mortgage\" followed by the tile shortcode.\n"); 
          print_string ("> "); st end
        else if cost <= List.assoc st.curr_player st.p_mon then begin
          let next_state = 
            {
              curr_player = st.curr_player;
              rolled = st.rolled;
              num_players = st.num_players;
              elim_players = st.elim_players;
              p_pos = st.p_pos;
              p_mon = change_money (0 - cost) st.curr_player st.p_mon;
              p_prop = st.p_prop;
              t_owner = st.t_owner;
              t_mortgages = (id,false)::(List.remove_assoc id st.t_mortgages);
              t_houses = st.t_houses;
              p_houses = st.p_houses;
              p_hotels = st.p_hotels;
              p_jail = st.p_jail;
              getoutcard = st.getoutcard;
              p_turns = st.p_turns
            } in
          print_board next_state (get_tile_info game);     
          print_endline("You've unmortgaged " ^ tile_name game id ^ " for a cost of $" ^ string_of_int cost ^ ".");
          print_endline("To mortgage a property, type \"mortgage\" followed by the tile shortcode.\n"); 
          print_string ("> "); next_state end
        else begin print_endline("You do not have enough money to unmortgage this property.\n"); 
          print_string ("> "); st end
      | _ -> print_endline("You can only unmortgage properties.\n"); 
        print_string("> "); st end
  | _ -> print_endline("You do not own this property so you cannot unmortgage it.\n"); 
    print_string("> "); st 


