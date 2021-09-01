(** The abstract type t representing a monopoly game containing
    a tile list, start tile, number of players, chance deck, 
    community chest deck, and initial money  *)
type t 

(** The type of card identifier*)
type card_id 

(** The type of data for storing board property name formatting and text *)
type tile_info = ANSITerminal.color*string

(** The type of function for a tile *)
type tile_function = Property of ANSITerminal.color | Chest | Chance | Income | Luxury | Go |
                     Railroad | Utilities | GoToJail | Jail | FreeParking  


(** Representation of a tile on the board *)
type tile

(** The type card contains an identifer, title, action, and a modifier for the
    action *) 
type card

(** The type of action for a card *)
type action = Money | GoToTile | MoveSpaces | GoJail | 
              GetOutJail | PropertyCharge


(**[from_json json] is the monopoly game from json file [json]*)
val from_json: Yojson.Basic.json -> t

(** [start_tile t] is the starting tile of the game [t] *)
val start_tile: t -> int 

(** [start_money t] is the starting money of the players in the game [t] *)
val start_money: t -> int 

(** [num_players t] returns the starting number of players in game t *)
val num_players : t -> int 

(** [tile_name t tile] is the name of the tile identified by [tile] 
    from the game [t] *)
val tile_name: t -> int -> string 

(** [tile_short t tile] is the shortten name of the tile identified by [tile] 
    from the game [t] *)
val tile_short: t -> int -> string 

(** [get_short_arr t] is the array of (style, short name) pairs for each
    property tile in the game [t] *)
val get_tile_info: t -> tile_info array

(** [tile_func t tile] is the function of the tile identified by [tile] 
    from the game [t] *)
val tile_func: t -> int -> tile_function

(** [tile_price t tile] is the price of the tile identified by [tile] 
    from the game [t] *)
val tile_price: t -> int -> int 

(** [tile_rent t tile] is the rent of the tile identified by [tile]
    in game [t] *)
val tile_rent: t -> int -> int

(** [tile_owner t tile] is the owner of the tile identified by [tile] 
    from the game [t] *)
val tile_owner: t -> int -> int

(** [tile_multirent t tile] is the rent of the tile identified by [tile]
    in game [t] *)
val tile_multirent: t -> int -> int list 

(** [tile_houses t tile] is the houses that can be owned 
    on the tile identified by [tile] from the game [t] *)
val tile_houses: t -> int -> int

(** [tile_hotels t tile] is the hotels that can be owned 
    on the tile identified by [tile] from the game [t] *)
val tile_hotels: t -> int -> int

(** [tile_house_cost t tile] is cost of houses for the tile identified by [tile] 
    from the game [t] *)
val tile_house_cost: t -> int -> int

(** [tile_hotel_cost t tile] is cost of hotels for the tile identified by [tile] 
    from the game [t] *)
val tile_hotel_cost: t -> int -> int

(** [get_chance_card game card_id] is a helper function to get the chance card 
    with identifier [card_id] in game [game] *)
val get_chance_card: t -> int -> card 

(** [tile_mortgaged game tile_id] is the mortgage state of the tile identified 
    by [tile_id] from the game [game] *)
val tile_mortgaged: t -> int -> bool

(** [get_comm_card game card_id] is a helper function to get the community chest 
    card with identifier [card_id] in game [game] *)
val get_comm_card : t -> int -> card 

(** [card_title card] gets the title of the card.  *)
val card_title : card -> string 

(** [card_action card] gets the action of the card.  *)
val card_action : card -> action 

(** [card_modi card] gets the modifier of the card.  *)
val card_modi : card -> int 

(** [start_money t] is the starting money of the players in the game [t] *)
val start_money: t -> int 

(** [num_players t] returns the starting number of players in game t *)
val num_players : t -> int 

(** [tile_short_to_id game short] returns None if short is not a tile 
    shortcode of any tile in game game, and returns Some n, where n
    is the id of the tile with shortcode short, if short is a
    shortcode for some tile in game game. *)
val tile_short_to_id : t -> string -> int option

(** [get_set game color] returns a list of all tiles in the set denoted
    by color in game game. *)
val get_set : t -> ANSITerminal.color -> int list
