open Yojson.Basic.Util

(** The type of card identifier*)
type card_id = int 

(** The type of data for storing board property name formatting and text *)
type tile_info = ANSITerminal.color*string

(** The type of function for a tile *)
type tile_function = Property of ANSITerminal.color | Chest | Chance | Income | Luxury | Go |
                     Railroad | Utilities | GoToJail | Jail | FreeParking  

(** The type of action for a card *)
type action = Money | GoToTile | MoveSpaces | GoJail | 
              GetOutJail | PropertyCharge

(** The type card contains an identifer, description, action, and a modifier 
    for the action 
    Example: action can be MoveSpaces and modifer can be 2, 
            the player moves 2 tiles ahead *)
type card = {
  id: int;
  title: string; 
  action: action;
  modifier: int 
}

(** The type tile contains an identifer, name, shortten name, function of 
    tile_function, price, owner, rent (for a7, multiple renting prices, 
    number of houses, hotels, cost of a house and hotel.). 
    Rent is the fee the player plays when landing another's owned property. *)
type tile = {
  id : int;
  name : string;
  short : string;
  func : tile_function;
  price : int;
  rent : int;
  multiplied_rent : int list;
  owner : int;
  houses : int;
  hotels : int;
  house_cost : int;
  hotel_cost : int;
  mortgaged : bool;
}

(** The abstract type t representing a monopoly game containing
    a tile list, start tile, number of players, chance deck, 
    community chest deck, and initial money  *)
type t = {
  tiles : tile list;
  tile_info : tile_info array;
  start_tile : int;
  num_players : int;
  chance : card list;
  community_chest : card list;
  init_money : int; 
}

(** [action_from_string s] is an action move for action [s]. 
    Example AddMoney is the card action to add money to player's pocket*)
let action_from_string (s:string) : action =
  match s with
  | "Money" -> Money
  | "GoToTile" -> GoToTile
  | "MoveSpaces" -> MoveSpaces
  | "GoJail" -> GoJail
  | "GetOutJail" -> GetOutJail 
  | "PropertyCharge" -> PropertyCharge
  | _ -> raise (Failure "Invalid action in JSON")

(** [card_of_json j] is a card from json file [j] containing identifer, title, 
    action, and modifier.  *)
let card_of_json (j: Yojson.Basic.json) : card = {
  id = j |> member "id" |> to_int;
  title = j |> member "title" |> to_string;
  action = j |> member "action" |> to_string |> action_from_string;
  modifier = j |> member "modifier" |> to_int
}

(** [function_from_string s] is the tile's function from function [s].
    Example: Some tiles are not property and cannot be bought, like Go. *)
let func_from_string (s: string) : tile_function = 
  match s with
  | "Purple" -> Property Black
  | "LightGreen" -> Property Red
  | "Violet" -> Property Green
  | "Orange" -> Property Yellow
  | "Red" -> Property Blue
  | "Yellow" -> Property Magenta
  | "DarkGreen" -> Property Cyan
  | "DarkBlue" -> Property White
  | "Chest" -> Chest
  | "Chance" -> Chance
  | "Income" -> Income
  | "Luxury" -> Luxury
  | "Go" -> Go
  | "Railroad" -> Railroad
  | "Utilities" -> Utilities
  | "GoToJail" -> GoToJail
  | "Jail" -> Jail
  | "FreeParking" -> FreeParking
  | _ -> raise (Failure "Invalid function in JSON")

(** [tile_of_json j] is a tile from json file [j] *)
let tile_of_json (j: Yojson.Basic.json) : tile =
  let func = j |> member "function" |> to_string |> func_from_string in
  {
    id = j |> member "id" |> to_int;
    name = j |> member "name" |> to_string;
    short = j |> member "short" |> to_string;
    func = func;
    price = j |> member "price" |> to_int;
    rent = j |> member "rent" |> to_int;
    multiplied_rent =  j |> member "multpliedrent" |> to_list |> List.map to_int; 
    owner = j |> member "owner" |> to_int;
    houses = 0;
    hotels = 0;
    house_cost = j |> member "house_cost" |> to_int;
    hotel_cost = j |> member "hotel_cost" |> to_int; 
    mortgaged = false;
  }

(**[from_json json] is the monopoly game from json file [json]*)
let from_json json = 
  let tiles = json |> member "tiles" |> to_list |> List.map tile_of_json in
  let chance = json |> member "chance" |> to_list |> List.map card_of_json in
  let community_chest = json |> member "community_chest" |> 
                        to_list |> List.map card_of_json in
  let tile_info =
    tiles 
    |> List.map (fun t -> match t.func with 
        |Property s -> (s, t.short) 
        | _ -> (Default, t.short)) 
    |> Array.of_list  in

  {
    tiles = tiles;
    tile_info = tile_info;
    start_tile = 0;
    num_players = 4;
    chance = chance;
    community_chest = community_chest;
    init_money = 1500;
  }

(** [start_tile game] is the starting tile of the game [game] *) 
let start_tile (game: t) : int = 
  game.start_tile 


(** [get_tile game id] is a helper function to get the tile with identifier 
    [tile_id] in game [game] *)
let get_tile (game: t) (tile_id : int) : tile = 
  let rec getter_tile lst = 
    match lst with 
    | [] -> failwith "Not possible to have unknown tile"
    | h::t -> if h.id = tile_id then h else getter_tile t 
  in getter_tile game.tiles 

(** [tile_name game tile_id] is the name of the tile identified by [tile_id] 
    from the game [game] *)
let tile_name (game: t) (tile_id: int) : string = 
  (get_tile game tile_id).name 

(** [tile_short game tile_id] is the shortened name of the tile identified by 
    [tile_id] from the game [game] *)
let tile_short (game: t) (tile_id: int) : string = 
  (get_tile game tile_id).short

(** [get_short_arr t tile] is the array of (style, short name) pairs for each
    property tile in the game [t] *)
let get_tile_info (t: t) : tile_info array = t.tile_info

(** [tile_func game tile_id] is the function of the tile identified by [tile_id] 
    from the game [game] *)
let tile_func (game: t) (tile_id: int) : tile_function = 
  (get_tile game tile_id).func 

(** [tile_price game tile_id] is the price of the tile identified by [tile_id] 
    from the game [game] *)
let tile_price (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).price

(** [tile_rent game tile_id] is the rent of the tile identified by [tile_id]
    in game [game] *)
let tile_rent (game:t) (tile_id: int) : int =
  (get_tile game tile_id).rent

(** [tile_owner game tile_id] is the owner of the tile identified by [tile_id] 
    from the game [game] *)
let tile_owner (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).owner 

(** [tile_multirent mt tile_id] is many rents of the tile identified by [tile_id]
    in game [game] *) 
let tile_multirent (game: t) (tile_id: int) : int list =
  (get_tile game tile_id).multiplied_rent

(** [tile_houses game tile_id] is the houses that can be owned 
    on the tile identified by [tile_id] from the game [game] *)
let tile_houses (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).houses 

(** [tile_hotels game tile_id] is the hotels that can be owned 
    on the tile identified by [tile_id] from the game [game] *)
let tile_hotels (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).hotels

(** [tile_house_cost game tile_id] is cost of houses for the tile identified 
    by [tile_id] from the game [game] *)
let tile_house_cost (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).house_cost

(** [tile_hotel_cost game tile_id] is cost of hotels for the tile identified 
    by [tile_id] from the game [game] *)
let tile_hotel_cost (game: t) (tile_id: int) : int = 
  (get_tile game tile_id).hotel_cost

(** [tile_mortgaged game tile_id] is the mortgage state of the tile identified 
    by [tile_id] from the game [game] *)
let tile_mortgaged (game: t) (tile_id: int) : bool = 
  (get_tile game tile_id).mortgaged

(** [getter_card card_id lst] is a helper function to match card list [lst]
    with card identifier [card_id]. *)
let rec getter_card (card_id: int) (lst : card list)  = 
  match lst with 
  | [] -> failwith "Not possible to have unknown card"
  | h::t -> if h.id = card_id then h else getter_card card_id t 

(** [get_chance_card game card_id] is a helper function to get the chance card 
    with identifier [card_id] in game [game] *)
let get_chance_card (game: t) (card_id : int) : card = 
  getter_card card_id game.chance

(** [get_comm_card game card_id] is a helper function to get the community chest 
    card with identifier [card_id] in game [game] *)
let get_comm_card (game: t) (card_id : int) : card = 
  getter_card card_id game.community_chest

let card_title (card : card) : string = 
  card.title 

let card_action (card : card) : action = 
  card.action 

let card_modi (card: card) : int = 
  card.modifier 

(** [start_money t] is the starting money of the players in the game [t] *)
let start_money (t: t) : int = 
  t.init_money

(** [num_players t] returns the starting number of players in game t *)
let num_players t = t.num_players 


(** [get_set_helper acc tile_list color] is a helper function used
    to return a list of all tiles in the set denoted by color. It is 
    called in get_set. *)
let rec get_set_helper acc tile_list (color:ANSITerminal.color) : int list = 
  match tile_list with 
  | [] -> acc 
  | h::t -> 
    begin match h.func with 
      | Property x when x = color -> get_set_helper ((h.id)::acc) t color
      | _ -> get_set_helper acc t color end 

(** [get_set game color] returns a list of all tiles in the set denoted
    by color in game game. *)
let get_set (game:t) (color:ANSITerminal.color) =
  get_set_helper [] game.tiles color

(** [tile_short_to_id game short] returns None if short is not a tile 
    shortcode of any tile in game game, and returns Some n, where n
    is the id of the tile with shortcode short, if short is a
    shortcode for some tile in game game. *)
let tile_short_to_id game short : int option =
  let rec helper lst s = 
    match lst with 
    | [] -> None
    | h::t -> if String.trim (h.short) = s then Some h.id else helper t s 
  in helper game.tiles short