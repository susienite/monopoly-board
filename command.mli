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

(** [parse str] parses a player's input into a [command].
    Raises: [Empty] is the string is empty.
            [Malformed] when the string is not a command. *)
val parse : string -> command