%%%-------------------------------------------------------------------
%%% @author Kuldeep <>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2016 by Kuldeep <>
%%%-------------------------------------------------------------------
-module(connack_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
get_pkt_1_test() ->
    SessionPresent = 0,
    ReturnCode = 0,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, 0:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_2_test() ->
    SessionPresent = 1,
    ReturnCode = 0,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, 0:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_3_test() ->
    SessionPresent = 1,
    ReturnCode = 1,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_4_test() ->
    SessionPresent = 1,
    ReturnCode = 2,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_5_test() ->
    SessionPresent = 1,
    ReturnCode = 3,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_6_test() ->
    SessionPresent = 0,
    ReturnCode = 4,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_7_test() ->
    SessionPresent = 0,
    ReturnCode = 5,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_8_test() ->
    SessionPresent = 0,
    % Generate random number between 6 and 255
    ReturnCodes = get_list_of_randoms(1000),
    [?assertEqual({error, 'illegal_return_code', ReturnCode}
		 ,connack:get_pkt({SessionPresent, ReturnCode}))
     || ReturnCode <- ReturnCodes].

get_list_of_randoms(Total) ->
    [6 + random:uniform(255 - 6) || _ <- lists:seq(1, Total)].
 
