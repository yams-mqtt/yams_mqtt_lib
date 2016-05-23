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
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_2_test() ->
    SessionPresent = 1,
    ReturnCode = 0,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_3_test() ->
    SessionPresent = 1,
    ReturnCode = 1,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_4_test() ->
    SessionPresent = 1,
    ReturnCode = 2,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_5_test() ->
    SessionPresent = 1,
    ReturnCode = 3,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_6_test() ->
    SessionPresent = 0,
    ReturnCode = 4,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

get_pkt_7_test() ->
    SessionPresent = 0,
    ReturnCode = 5,
    ?assertEqual(<<2:4, 0:4, 2:8, 0:7, SessionPresent:1, ReturnCode:8>>,
		 connack:get_pkt({SessionPresent, ReturnCode})).

