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
type t 

(** [init_state game] returns the initial state of the game specified in 
    monopoly.t game *)
val init_state : Monopoly.t -> int -> t 

(** [curr_player st] returns the current player in state st. *)
val curr_player: t -> int 

(** [player_pos st] returns the player to position association list
    p_pos in state st. *)
val player_pos : t -> (int*int) list

(** [num_players st] returns the number of players initally in the state [st]*)
val num_players : t -> int 

(* [elim_player_lst st] returns the eliminated players in state st. *)
val elim_player_lst : t -> int list

(** [get_properties id st] returns the list of properties owned by player id. *)
val get_properties : int -> t -> int list  

(** [get_pocket id st] returns the amount of money owned by player id. *)
val get_pocket : int -> t -> int 

(** [get_tile_owner id st] returns the owner of the tile with id id in
    game with current state st. *)
val get_tile_owner: int -> t -> int 

(** [get_t_houses id st] returns the number of houses on tile with [id] in
    game with current state [st]. *)
val get_t_houses: int -> t -> int

(** [get_outcard id st] returns whether the player [id] owns a get out jail card in 
    game with current state [st]. *)
val get_outcard : int -> t -> bool 

(** [get_outcard id st] returns whether the player [id] is in jail in 
    game with current state [st]. *)
val get_rolled : t -> bool 

(** [get_p_houses id st] returns the houses associated with player [id] in
    game with current state [st]. *)
val get_jail_bool : int -> t -> bool 

(** [get_p_houses id st] returns the houses associated with player [id] in
    game with current state [st]. *)
val get_p_houses : int -> t -> int

(** [get_p_hotels id st] returns the hotels associated with player [id] in
    game with current state [st]. *)
val get_p_hotels: int -> t -> int 

(** [get_mortgaged_bool id st] returns whether the tile [id] is mortgaged in 
    game with current state [st]. *)
val get_mortgaged_bool: int -> t -> bool

(** [roll st game] advances the current player a random number of tiles
    between 1 and 12. *)
val roll : t -> Monopoly.t -> t 

(** [rolln st game n ] advances the current player n tiles in game game with
    current state st. *)
val rolln : t -> Monopoly.t -> int -> t

(** [player_eliminated st owner] is a state where the current player [player] 
    gives all his values to owner [owner] *)
val player_eliminated : t -> int -> t

(** [pay_rent st game] transfers an amount of money equal to the rent of the
    tile that the current player is on to the owner of the tile if the tile
    is owned by a player that is not the current player. *)
val pay_rent : t -> Monopoly.t -> int -> t

(** [buy st game] adds the tile that the current player is on the player's
    list of properties and changes the tile's owner to the current player,
    if it is possible for the current player to buy the current tile
    (i.e., the player has enough money, and the tile is a tile that
    can be bought). *)
val buy : t -> Monopoly.t -> t

(** [next_turn game st t] advances the state of the game to the next
    player's turn. *)
val next_turn : t -> Monopoly.t -> t

(** [print_board st c] dynamically prints the current state of the board. It 
    uses arrays to keep track of key elements such as player positions 
    and tile owners *)
val print_board : t -> (Monopoly.tile_info array) -> unit

(** [choose_chance st game] is the new state when a choosen card from chance
    is played in current state [st] in game [game]. *)
val choose_chance : t -> Monopoly.t -> int -> t 

(** [choose_comm st game] is the new state when a choosen card from community
    chest is played in current state [st] in game [game]. *)
val choose_comm : t -> Monopoly.t -> int -> t 

(** [pay_jail st game] is the new state when the player pays the $50 jail fee
    in current state [st] in game [game]. If the player does not have enough 
    money, then the player is in the state [cant_play st game].*)
val pay_jail : t -> Monopoly.t -> t 

(** [jail_card st game] returns a new state when the player applies the Get Out of
    Jail card in current state [st] in game [game].*)
val jail_card: t -> Monopoly.t -> t 

(** [buy_house st game id] adds a house to tile id in game [game] with
    current state [st].*)
val buy_house : t -> Monopoly.t -> int -> t

(** [buy_hotel st game id] adds a hotel to and removes four houses from 
    tile id in game [game] with current state [st]. *)
val buy_hotel : t -> Monopoly.t -> int -> t

(** [own_set st game player s] is a helper function that determines if
    player p owns every property in the set s in game game with current
    state st. *)
val own_set: t -> Monopoly.t -> int -> ANSITerminal.color -> bool 

(** [sell_house st game id] removes a house from tile [id] and adds the
    cost of the house to the current player's account. *)
val sell_house : t -> Monopoly.t -> int -> t 

(** [sell hotel st game id] removes a hotel from tile id in game [game] with
    state [st] and adds the cost to the current player's pocket if the owner of 
    the tile is the current player and the tile has a hotel on it. After
    selling the hotel, the tile will have four houses. *)
val sell_hotel : t -> Monopoly.t -> int -> t

(** [mortgage st game id] mortgages tile [id], selling all properties from
    tiles of the same for half their value and returning half the value of the, 
    property. Hotels and houses can no longer be bought all mortgages lifted. *)
val mortgage : t -> Monopoly.t -> int -> t 

(** [unmortgage st game id] unmortgages tile [id], removing the original 
    mortgage raise and 10% interest from the players pocket and enabling
    the purchase of houses and hotels on all tiles of the same color. *)
val unmortgage : t -> Monopoly.t -> int -> t

val get_rolled : t -> bool

val get_jail_bool : int -> t -> bool 
