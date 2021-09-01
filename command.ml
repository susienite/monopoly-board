(** The type [object_phrase] represents the object phrase that can be part 
    of a player command. *)
type object_phrase = string list

(** The type [command] represents a player command . *)
type command =
  |Roll
  |Rolln of int
  |Tile  
  |Status 
  |Buy 
  |Chance of int 
  |Chest of int 
  |GetOutCard
  |JailFee 
  |BuyHouse of string
  |BuyHotel of string
  |SellHouse of string 
  |SellHotel of string
  |Mortgage of string
  |UnMortgage of string
  |End
  |Quit
  |Help

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [str_to_strlist str] converts a string to a string list, seperated on spaces.*)
let str_to_strlist (str:string) : string list =
  String.split_on_char ' ' str

(** [remove_spaces strlist] removes all whitespace elements from a string list.*)
let rec remove_spaces (strlist:string list) : string list =
  match strlist with
  | [] -> []
  | hd::tl -> if hd = "" then remove_spaces tl else hd::remove_spaces tl

(** [parse str] parses a player's input into a [command].
    Raises: [Empty] is the string is empty.
            [Malformed] when the string is not a command. *)
let parse (possible_cmd:string) : command =
  let strlist = str_to_strlist possible_cmd in
  let rem_space_cmd = remove_spaces strlist in
  match rem_space_cmd with  
  | [] -> raise Empty
  | ["roll"] -> Roll
  | ("rolln")::[x] -> Rolln (int_of_string x)
  | ["tile"] -> Tile 
  | ["status"] -> Status 
  | ("buy")::("house")::[x] -> BuyHouse x 
  | ("buy")::("hotel")::[x] -> BuyHotel x
  | ("sell")::("house")::[x] -> SellHouse x 
  | ("sell")::("hotel")::[x] -> SellHotel x
  | ["buy"] -> Buy 
  | ("chance")::[x] -> Chance (int_of_string x) 
  | ("chest")::[x] -> Chest (int_of_string x) 
  | ("get")::("out")::("card")::[] -> GetOutCard 
  | ("pay")::("jail")::("fee")::[] -> JailFee
  | ("mortgage")::[s] -> Mortgage s
  | ("unmortgage")::[s] -> UnMortgage s
  | ["end"] -> End
  | ["help"] -> Help
  | ["quit"] -> Quit
  | _ -> raise Malformed

