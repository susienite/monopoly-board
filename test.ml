open OUnit2
open Monopoly
open State
open Command

let mp = Yojson.Basic.from_file "board.json"
let a6_mp = from_json mp 
let init_mp = init_state a6_mp 4
let init_mp_1 = init_state a6_mp 3
let init_mp_2 = init_state a6_mp 2
let chance_card_0 = get_chance_card a6_mp 0
let chest_card_1 = get_comm_card a6_mp 1


let mp2 = Yojson.Basic.from_file "monopoly.json"
let a6_mp2 = from_json mp2
let init_mp2 = init_state a6_mp 4
let init_mp2_1 = init_state a6_mp 3
let init_mp2_2 = init_state a6_mp 2
let chance_mp2_0 = get_chance_card a6_mp2 0
let chest_mp2_1 = get_comm_card a6_mp2 1

let monopoly_tests =
  [ 
    "board.json tile name" >:: (fun _ -> 
        assert_equal "Townhouses" (tile_name a6_mp 1));
    "board.json tile short name" >:: (fun _ -> 
        assert_equal " TOWNHOUS " (tile_short a6_mp 1));
    "board.json tile function" >:: (fun _ -> 
        assert_equal (Property Black) (tile_func a6_mp 1));
    "board.json tile price" >:: (fun _ -> 
        assert_equal 60 (tile_price a6_mp 1));
    "board.json tile rent" >:: (fun _ -> 
        assert_equal 2 (tile_rent a6_mp 1));
    "board.json tile multiple rent" >:: (fun _ -> 
        assert_equal ([10; 30; 90; 160; 250]) (tile_multirent a6_mp 1));
    "board.json tile owner" >:: (fun _ -> 
        assert_equal 0 (tile_owner a6_mp 1));
    "board.json tile houses" >:: (fun _ -> 
        assert_equal 0 (tile_houses a6_mp 1));
    "board.json tile hotels" >:: (fun _ -> 
        assert_equal 0 (tile_hotels a6_mp 1));
    "board.json tile house cost" >:: (fun _ -> 
        assert_equal 50 (tile_house_cost a6_mp 1));
    "board.json tile hotel cost" >:: (fun _ -> 
        assert_equal 60 (tile_hotel_cost a6_mp 1));
    "board.json chance card title" >:: (fun _ -> 
        assert_equal "Advance to Go (Collect $200)" (card_title chance_card_0));
    "board.json chance card action" >:: (fun _ -> 
        assert_equal GoToTile (card_action chance_card_0));
    "board.json chance card modifier" >:: (fun _ -> 
        assert_equal 0 (card_modi chance_card_0));
    "board.json community chest title" >:: (fun _ -> 
        assert_equal "Tuition error in your favor - Collect $200 "
          (card_title chest_card_1));
    "board.json chest card action" >:: (fun _ -> 
        assert_equal Money (card_action chest_card_1));
    "board.json chest card modifier" >:: (fun _ -> 
        assert_equal 200 (card_modi chest_card_1));
    "board.json start money" >:: (fun _ -> 
        assert_equal 1500 (start_money a6_mp));
    "board.json tile id from short name" >:: (fun _ -> 
        assert_equal (Some 1) (tile_short_to_id a6_mp "TOWNHOUS"));


    "monopoly.json tile name" >:: (fun _ -> 
        assert_equal "Mediterranean" (tile_name a6_mp2 1));
    "monopoly.json tile short name" >:: (fun _ -> 
        assert_equal " MEDITERR " (tile_short a6_mp2 1));
    "monopoly.json tile function" >:: (fun _ -> 
        assert_equal (Property Black) (tile_func a6_mp2 1));
    "monopoly.json tile price" >:: (fun _ -> 
        assert_equal 60 (tile_price a6_mp2 1));
    "monopoly.json tile rent" >:: (fun _ -> 
        assert_equal 2 (tile_rent a6_mp2 1));
    "monopoly.json tile multiple rent" >:: (fun _ -> 
        assert_equal ([10; 30; 90; 160; 250]) (tile_multirent a6_mp2 1));
    "monopoly.json tile owner" >:: (fun _ -> 
        assert_equal 0 (tile_owner a6_mp2 1));
    "monopoly.json tile houses" >:: (fun _ -> 
        assert_equal 0 (tile_houses a6_mp2 1));
    "monopoly.json tile hotels" >:: (fun _ -> 
        assert_equal 0 (tile_hotels a6_mp2 1));
    "monopoly.json tile house cost" >:: (fun _ -> 
        assert_equal 50 (tile_house_cost a6_mp2 1));
    "monopoly.json tile hotel cost" >:: (fun _ -> 
        assert_equal 100 (tile_hotel_cost a6_mp2 1));
    "monopoly.json chance card title" >:: (fun _ -> 
        assert_equal "Advance to Go (Collect $200)" (card_title chance_mp2_0));
    "monopoly.json chance card action" >:: (fun _ -> 
        assert_equal GoToTile (card_action chance_mp2_0));
    "monopoly.json chance card modifier" >:: (fun _ -> 
        assert_equal 0 (card_modi chance_mp2_0));
    "monopoly.json community chest title" >:: (fun _ -> 
        assert_equal "Bank error in your favor—Collect $200"
          (card_title chest_mp2_1));
    "monopoly.json chest card action" >:: (fun _ -> 
        assert_equal Money (card_action chest_mp2_1));
    "monopoly.json chest card modifier" >:: (fun _ -> 
        assert_equal 200 (card_modi chest_mp2_1));
    "monopoly.json start money" >:: (fun _ -> 
        assert_equal 1500 (start_money a6_mp2));
    "monopoly.json tile id from short name None" >:: (fun _ -> 
        assert_equal (None) (tile_short_to_id a6_mp2 " None "));
    "monopoly.json tile id from short name Some" >:: (fun _ -> 
        assert_equal (Some 1) (tile_short_to_id a6_mp2 "MEDITERR"));
  ]

let com1 = "roll"
let com2 = " rolln  1"
let com3 = " tile"
let com4 = " status"
let com5 = "buy "
let com6 = "chance 1 "
let com7 = "chest 1      "
let com8 = "    get out card "
let com9 = "  pay    jail      fee "
let com10 = "  buy house TOWNHOUS "
let com11 = "  buy hotel TOWNHOUS "
let com12 = "  sell house TOWNHOUS "
let com13 = "  sell hotel TOWNHOUS "
let com14 = " help "
let com15 = "  end  "
let com16 = "  quit  "
let com17 = "  rolln 40  "
let com18 = " rolln 50  "
let com19 = " chance 15  "
let com20 = " chest 15  "
let com21 = "  buy house       LOWRISES  "
let com22 = "  buy hotel    LOWRISES  "
let com23 = "  sell house       LOWRISES  "
let com24 = "  sell hotel    LOWRISES  "


let make_command_tests
    (name : string)
    (input : string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse (input)))

let command_tests = [
  make_command_tests "roll test" com1 Roll;
  make_command_tests "rolln test" com2 (Rolln 1);
  make_command_tests "rolln test" com17 (Rolln 40);
  make_command_tests "rolln test" com18 (Rolln 50);
  make_command_tests "tile test" com3 Tile;
  make_command_tests "status test" com4 Status;
  make_command_tests "buy test" com5 Buy;
  make_command_tests "chance 1 test" com6 (Chance 1);
  make_command_tests "chance 1 test" com19 (Chance 15);
  make_command_tests "chest 2 test" com7 (Chest 1);
  make_command_tests "chest 2 test" com20 (Chest 15);
  make_command_tests "get out test" com8 (GetOutCard);
  make_command_tests "pay jail fee test" com9 (JailFee);
  make_command_tests "buy house test" com10 (BuyHouse "TOWNHOUS");
  make_command_tests "buy hotel test" com11 (BuyHotel "TOWNHOUS");
  make_command_tests "sell house test" com12 (SellHouse "TOWNHOUS");
  make_command_tests "sell hotel test" com13 (SellHotel "TOWNHOUS");
  make_command_tests "buy house test" com21 (BuyHouse "LOWRISES");
  make_command_tests "buy hotel test" com22 (BuyHotel "LOWRISES");
  make_command_tests "sell house test" com23 (SellHouse "LOWRISES");
  make_command_tests "sell hotel test" com24 (SellHotel "LOWRISES");
  make_command_tests "help test" com14 Help;
  make_command_tests "end test" com15 End;
  make_command_tests "quit test" com16 Quit;
  "empty input" >:: (fun _ -> 
      assert_raises (Empty) (fun() -> parse ""));
  "invalid input" >:: (fun _ ->
      assert_raises (Malformed) (fun() -> parse "   Malformed  "));
  "invalid rolln" >:: (fun _ ->
      assert_raises (Malformed) (fun() -> parse "   rolln10 "));
]


let state_tests = [
  "board.json curr_player" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp));
  "board.json num_player" >:: (fun _ -> 
      assert_equal 4 (num_players init_mp));
  "board.json player_pos" >:: (fun _ -> 
      assert_equal [(1,0);(2,0);(3,0);(4,0)] (player_pos init_mp));
  "board.json elim_play lst" >:: (fun _ -> 
      assert_equal [] (elim_player_lst init_mp));
  "board.json get_properties" >:: (fun _ -> 
      assert_equal [] (get_properties 1 init_mp));
  "board.json get_pocket" >:: (fun _ -> 
      assert_equal 1500 (get_pocket 1 init_mp));
  "board.json init_house" >:: (fun _ ->
      assert_equal 0 (get_t_houses 1 init_mp));
  "board.json tile_owner" >:: (fun _ ->
      assert_equal 0 (get_tile_owner 1 init_mp));
  "board.json get out jail card" >:: (fun _ ->
      assert_equal false (get_outcard 1 init_mp));
  "board.json get jail bool" >:: (fun _ ->
      assert_equal false (get_jail_bool 1 init_mp));
  "board.json get player houses" >:: (fun _ ->
      assert_equal 0 (get_p_houses 1 init_mp));
  "board.json get player hotels" >:: (fun _ ->
      assert_equal 0 (get_p_hotels 1 init_mp));
  "board.jsonget rolled test1" >:: (fun _ -> 
      assert_equal false (get_rolled init_mp));


  "board.json curr_player test1" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp_1));
  "board.json num_player test1" >:: (fun _ -> 
      assert_equal 3 (num_players init_mp_1));
  "board.json player_pos test1" >:: (fun _ -> 
      assert_equal [(1,0);(2,0);(3,0)] (player_pos init_mp_1));
  "board.json curr_player test2" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp_2));
  "board.json num_player test2" >:: (fun _ -> 
      assert_equal 2 (num_players init_mp_2));
  "board.json player_pos test2" >:: (fun _ -> 
      assert_equal [(1,0);(2,0)] (player_pos init_mp_2));


  "curr_player" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp2));
  "player_pos" >:: (fun _ -> 
      assert_equal [(1,0);(2,0);(3,0);(4,0)] (player_pos init_mp2));
  "elim_play lst" >:: (fun _ -> 
      assert_equal [] (elim_player_lst init_mp2));
  "get_properties test1" >:: (fun _ -> 
      assert_equal [] (get_properties 1 init_mp2));
  "get_pocket" >:: (fun _ -> 
      assert_equal 1500 (get_pocket 1 init_mp2));
  "init_house" >:: (fun _ ->
      assert_equal 0 (get_t_houses 1 init_mp2));
  "tile_owner" >:: (fun _ ->
      assert_equal 0 (get_tile_owner 1 init_mp2));
  "get out jail card" >:: (fun _ ->
      assert_equal false (get_outcard 1 init_mp2));
  "get jail bool" >:: (fun _ ->
      assert_equal false (get_jail_bool 1 init_mp2));
  "get player houses" >:: (fun _ ->
      assert_equal 0 (get_p_houses 1 init_mp2));
  "get player hotels" >:: (fun _ ->
      assert_equal 0 (get_p_hotels 1 init_mp2));
  "get rolled" >:: (fun _ -> 
      assert_equal false (get_rolled init_mp2));
  "number of players" >:: (fun _ -> 
      assert_equal 4 (num_players init_mp2));

  "monopoly.json curr_player test1" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp2_1));
  "monopoly.json num_player test1" >:: (fun _ -> 
      assert_equal 3 (num_players init_mp2_1));
  "monopoly.json player_pos test1" >:: (fun _ -> 
      assert_equal [(1,0);(2,0);(3,0)] (player_pos init_mp2_1));
  "monopoly.json curr_player test2" >:: (fun _ -> 
      assert_equal 1 (curr_player init_mp2_2));
  "monopoly.json num_player test2" >:: (fun _ -> 
      assert_equal 2 (num_players init_mp2_2));
  "monopoly.json player_pos test2" >:: (fun _ -> 
      assert_equal [(1,0);(2,0)] (player_pos init_mp2_2));
] 

let suite = 
  "test suite for A6" >::: List.flatten [
    command_tests;
    monopoly_tests;
    state_tests
  ]

let _ = run_test_tt_main suite




