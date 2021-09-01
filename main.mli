   (*

(**[pp_color s] is the prettified string for the property color [s]. *)
val pp_color: ANSITerminal.color -> string

(** [print_properties game st] prints the properties owned by player 
    in state [st] in game [game] *)
val print_properties: Monopoly.t -> State.t -> string

(** [print_players st] prints the players currently in the state [st] *)
val print_players: State.t -> string

(**[print_tile_func game st] prints the tile function of the current tile in
   	   the state [st] in the game [game]. *)
val print_tile_func: Monopoly.t -> State.t -> string

(**[print_owner game st] prints the owner of the current tile in
   	   the state [st] in the game [game]. *)
val print_owner: Monopoly.t -> State.t -> string

(**[print_rent game st] prints the rent of the current tile in
   	the state [st] in the game [game]. *)
val print_rent: Monopoly.t -> State.t -> string

   (**[play_chance_card game st card] is the outcome of grabbing a chance card in 
   state [st] in the game [game]. *)
   val play_chance_card: Monopoly.t -> State.t -> Monopoly.card -> State.t

   (**[play_comm_card game st card] is the outcome of grabbing a community chest
   card in state [st] in the game [game]. 
   val play_comm_card: Monopoly.t -> State.t -> Monopoly.card -> State.t *)

val print_rent: Monopoly.t -> State.t -> string

(** [play_game f] starts the game in file [f]. 
    Example: play_game monopoly.json starts the monopoly game. 
    Raises: exception when an invalid file name is given. *)
val play_game: string -> unit 

(** [main ()] prompts for the game to play, then starts it. 
    Example: Entering monopoly.json starts the monopoly game.*)
val main: unit -> unit  

*)