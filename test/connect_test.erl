%%%-------------------------------------------------------------------
%%% @author Kuldeep <kuldeep@ThinkErl>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Kuldeep <kuldeep@ThinkErl>
%%%-------------------------------------------------------------------
-module(connect_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/connect.hrl").

%%%===================================================================
%%% Tests : parse
%%%===================================================================
parse_with_id_test()->
?assertEqual({ok, 
	      #conn_vh{protocol = "MQTT", level = 4, connflgs = {0, 0, 0, 0, 0, 0, 0}, kat = 1}, 
	      #conn_pl{id = "abcde", wt = "", wm = "", usr = "", pwd = ""}
	     },
	     connect:parse(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 5:16, "abcde">>)).

parse_with_invalid_proto_lvl_test()->
?assertEqual({error,connack1, <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 5:16, "abcde">>},
	     connect:parse(<<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 5:16, "abcde">>)).

parse_with_id_wt_wm_test()->
?assertEqual({ok, 
	      #conn_vh{protocol = "MQTT", level = 4, connflgs = {0, 0, 0, 0, 1, 0, 0}, kat = 1}, 
	      #conn_pl{id = "abcde", wt = "ab", wm = "ab", usr = "", pwd = ""}
	     },
	     connect:parse(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 5:16, "abcde", 2:16, "ab", 2:16, "ab">>)).

parse_with_id_wt_wm_usr_test()->
?assertEqual({ok, 
	      #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 0, 0, 0, 1, 0, 0}, kat = 1}, 
	      #conn_pl{id = "abcde", wt = "ab", wm = "ab", usr = "ab", pwd = ""}
	     },
	     connect:parse(<<4:16, "MQTT", 4:8, 1:1, 0:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 5:16, "abcde", 2:16, "ab", 2:16, "ab", 2:16, "ab">>)).

parse_with_id_wt_wm_usr_pwd_test()->
?assertEqual({ok, 
	      #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 0, 1, 0, 0}, kat = 1}, 
	      #conn_pl{id = "abcde", wt = "ab", wm = "ab", usr = "ab", pwd = "xy"}
	     },
	     connect:parse(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 5:16, "abcde", 2:16, "ab", 2:16, "ab", 2:16, "ab", 2:16, "xy">>)).

parse_with_pl_too_long_test()->
?assertEqual({error,payload_too_long,<<"z">>},
	     connect:parse(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 5:16, "abcde", 2:16, "ab", 2:16, "ab", 2:16, "ab", 2:16, "xyz">>)).

parse_without_id_pwd_test()->
?assertEqual({error,invalid_password,<<>>},
	     connect:parse(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 5:16, "abcde", 2:16, "ab", 2:16, "ab", 2:16, "ab">>)).
%%%===================================================================
%%% Tests : get_vh
%%%===================================================================
get_vh_valid_proto_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {0, 0, 0, 0, 0, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_usrpwd_10_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 0, 0, 0, 0, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_usrpwd_11_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 0, 0, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_100_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 0, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_110_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 1, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 1:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_120_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 2, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 2:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_101_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 0, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 0:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_111_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 1, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 1:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_valid_wwqwr_121_test() ->
?assertEqual({ok, #conn_vh{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 2, 1, 0, 0}, kat = 1}, <<100:16>>},
	     connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 2:2, 1:1, 0:1, 0:1, 1:16, 100:16>>)).

get_vh_invalid_proto_size_test() ->
?assertEqual({error, 'invalid_proto', <<5:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
	     (connect:get_vh(<<5:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_proto_name_test() ->
?assertEqual({error, 'invalid_proto', <<4:16, "mqtt", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "mqtt", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_proto_lvl_test() ->
?assertEqual({error, 'connack1', <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_no_conn_flgs_test() ->
?assertEqual({error, 'no_conn_flgs', <<4:16, "MQTT", 4:8>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8>>))).

get_vh_invalid_reserved_flg_test() ->
?assertEqual({error, 'invalid_rsvd_flg', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 1:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 1:1, 1:16, 100:16>>))).

get_vh_invalid_will_qos_test() ->
?assertEqual({error, 'invalid_will_qos', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_will_1_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 1:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 1:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_will_2_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 2:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 2:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_will_3_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 0:1, 1:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 0:1, 1:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_will_4_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 1:1, 2:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 1:1, 2:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_pwd_flg_test() ->
?assertEqual({error, 'invalid_pwd_flg', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_vh_invalid_kat_1_test() ->
?assertEqual({error, 'invalid_kat', <<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:8>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:8>>))).

get_vh_invalid_kat_2_test() ->
?assertEqual({error, 'invalid_kat', <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1>>))).

%%%===================================================================
%%% Tests : get_proto
%%%===================================================================
get_proto_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_proto_invalid_size_test() ->
    ?assertEqual({error, 'invalid_proto', <<5:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto(<<5:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_proto_invalid_case_1_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "Mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto(<<4:16, "Mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_proto_invalid_case_2_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto(<<4:16, "mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

get_proto_invalid_name_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto(<<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>))).

%%%===================================================================
%%% Tests : get_proto_lvl
%%%===================================================================

get_proto_lvl_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

get_proto_lvl_invalid_1_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 3:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 3:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

get_proto_lvl_invalid_2_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

get_proto_lvl_invalid_3_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 255:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 255:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

get_proto_lvl_invalid_4_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 0:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 0:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).


get_proto_lvl_err_in_err_out_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_proto_lvl({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).


%%%===================================================================
%%% Tests : get_rsvd_flg
%%%===================================================================

get_rsvd_flg_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

get_rsvd_flg_invalid_flg_test() ->
    ?assertEqual({error, 'invalid_rsvd_flg', <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 1:1, 1:16, 100:16>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 1:1, 1:16, 100:16>>}))).

get_rsvd_flg_no_conn_flg_test() ->
    ?assertEqual({error, 'no_conn_flgs', <<4:16, "MQTT", 4:8>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8>>}))).

get_rsvd_flg_err_in_err_out_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>},
		 (connect:get_reserved_flg({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16, 100:16>>}))).

%%%===================================================================
%%% Tests : get_client_id
%%%===================================================================
get_client_id_valid_test() ->
    ?assertEqual({ok, {0, 0, 0, 0, 0, 0, 0}, <<>>, #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""}},
		 (connect:get_client_id({ok, {0, 0, 0, 0, 0, 0, 0}, <<3:16, "abc">>}))).

get_client_id_invalid_test() ->
    ?assertEqual({error,invalid_client_id, {0, 0, 0, 0, 0, 0, 0}, <<3:16, "ab">>},
		 (connect:get_client_id({ok, {0, 0, 0, 0, 0, 0, 0}, <<3:16, "ab">>}))).

%%%===================================================================
%%% Tests : get_will_topic
%%%===================================================================
get_will_topic_valid_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<>>, ConnPl#conn_pl{ wt = "abcde!@#$%" }},
		 (connect:get_will_topic({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%">>, ConnPl}))).

get_will_topic_valid_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<"xyz">>, ConnPl#conn_pl{ wt = "abcde!@#$%" }},
		 (connect:get_will_topic({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%xyz">>, ConnPl}))).

get_will_topic_valid_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<3:16, "xyz">>, ConnPl#conn_pl{ wt = "abcde!@#$%" }},
		 (connect:get_will_topic({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%", 3:16, "xyz">>, ConnPl}))).

get_invalid_will_topic_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_topic, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_will_topic({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>, ConnPl}))).

get_will_topic_when_error_passed_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_client_id, {0, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>},
		 (connect:get_will_topic({error,invalid_client_id, {0, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>}))).

%%%===================================================================
%%% Tests : get_will_msg
%%%===================================================================
get_will_msg_valid_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<>>, ConnPl#conn_pl{ wm = "abcde!@#$%" }},
		 (connect:get_will_msg({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%">>, ConnPl}))).

get_will_msg_valid_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<"xyz">>, ConnPl#conn_pl{ wm = "abcde!@#$%" }},
		 (connect:get_will_msg({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%xyz">>, ConnPl}))).

get_will_msg_valid_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {0, 0, 0, 0, 1, 0, 0}, <<3:16, "xyz">>, ConnPl#conn_pl{ wm = "abcde!@#$%" }},
		 (connect:get_will_msg({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%", 3:16, "xyz">>, ConnPl}))).

get_invalid_will_msg_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_msg, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_will_msg({ok, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>, ConnPl}))).

get_will_msg_when_error_passed_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_client_id, {0, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>},
		 (connect:get_will_msg({error,invalid_client_id, {0, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>}))).

get_will_msg_when_error_passed_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_topic, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_will_msg({error,invalid_will_topic, {0, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

%%%===================================================================
%%% Tests : get_usr
%%%===================================================================
get_usr_valid_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "abcde!@#$%", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 0, 0, 0, 1, 0, 0}, <<>>, ConnPl#conn_pl{ usr = "abcde!@#$%" }},
		 (connect:get_usr({ok, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%">>, ConnPl}))).

get_usr_valid_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "abcde!@#$%", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 0, 0, 0, 1, 0, 0}, <<"xyz">>, ConnPl#conn_pl{ usr = "abcde!@#$%" }},
		 (connect:get_usr({ok, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%xyz">>, ConnPl}))).

get_usr_valid_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 0, 0, 0, 0, 0, 0}, <<3:16, "xyz">>, ConnPl#conn_pl{ usr = "abcde!@#$%" }},
		 (connect:get_usr({ok, {1, 0, 0, 0, 0, 0, 0}, <<10:16, "abcde!@#$%", 3:16, "xyz">>, ConnPl}))).

get_invalid_usr_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_user, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_usr({ok, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>, ConnPl}))).

get_usr_when_error_passed_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_client_id, {1, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>},
		 (connect:get_usr({error,invalid_client_id, {1, 0, 0, 0, 0, 0, 0}, <<3:10, "ab">>}))).

get_usr_when_error_passed_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_topic, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_usr({error,invalid_will_topic, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

get_usr_when_error_passed_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_msg, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_usr({error,invalid_will_msg, {1, 0, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

%%%===================================================================
%%% Tests : get_pwd
%%%===================================================================
get_pwd_valid_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "abcde!@#$%", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 1, 0, 0, 1, 0, 0}, <<>>, ConnPl#conn_pl{ pwd = "abcde!@#$%" }},
		 (connect:get_pwd({ok, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%">>, ConnPl}))).

get_pwd_valid_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "abcde!@#$%", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 1, 0, 0, 1, 0, 0}, <<"xyz">>, ConnPl#conn_pl{ pwd = "abcde!@#$%" }},
		 (connect:get_pwd({ok, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$%xyz">>, ConnPl}))).

get_pwd_valid_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({ok, {1, 1, 0, 0, 0, 0, 0}, <<3:16, "xyz">>, ConnPl#conn_pl{ pwd = "abcde!@#$%" }},
		 (connect:get_pwd({ok, {1, 1, 0, 0, 0, 0, 0}, <<10:16, "abcde!@#$%", 3:16, "xyz">>, ConnPl}))).

get_invalid_pwd_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "abcde!@#$%", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_password, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_pwd({ok, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>, ConnPl}))).

get_pwd_when_error_passed_1_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_client_id, {1, 1, 0, 0, 0, 0, 0}, <<3:10, "ab">>},
		 (connect:get_pwd({error,invalid_client_id, {1, 1, 0, 0, 0, 0, 0}, <<3:10, "ab">>}))).

get_pwd_when_error_passed_2_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_topic, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_pwd({error,invalid_will_topic, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

get_pwd_when_error_passed_3_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_will_msg, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_pwd({error,invalid_will_msg, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

get_pwd_when_error_passed_4_test() ->
    ConnPl = #conn_pl{id = "abc", wt = "", wm = "", usr = "", pwd = ""},
    ?assertEqual({error,invalid_user, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>},
		 (connect:get_pwd({error,invalid_user, {1, 1, 0, 0, 1, 0, 0}, <<10:16, "abcde!@#$">>}))).

