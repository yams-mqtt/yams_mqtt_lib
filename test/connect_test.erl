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
%%% Tests : get_vh
%%%===================================================================
get_vh_valid_proto_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {0, 0, 0, 0, 0, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_usrpwd_10_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 0, 0, 0, 0, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_usrpwd_11_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 0, 0, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_100_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 0, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 0:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_110_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 1, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 1:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_120_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 0, 2, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 0:1, 2:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_101_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 0, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 0:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_111_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 1, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 1:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_valid_wwqwr_121_test() ->
?assertEqual({ ok 
	     , #var_head{protocol = "MQTT", level = 4, connflgs = {1, 1, 1, 2, 1, 0, 0}}
	     , <<1:16>>
	     }
	    , connect:get_vh(<<4:16, "MQTT", 4:8, 1:1, 1:1, 1:1, 2:2, 1:1, 0:1, 0:1, 1:16>>)).

get_vh_invalid_proto_size_test() ->
?assertEqual({error, 'invalid_proto', <<5:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<5:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_proto_name_test() ->
?assertEqual({error, 'invalid_proto', <<4:16, "mqtt", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "mqtt", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_proto_lvl_test() ->
?assertEqual({error, 'connack1', <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_no_conn_flgs_test() ->
?assertEqual({error, 'no_conn_flgs', <<4:16, "MQTT", 4:8>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8>>))).

get_vh_invalid_reserved_flg_test() ->
?assertEqual({error, 'invalid_rsvd_flg', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 1:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 1:1, 1:16>>))).

get_vh_invalid_will_qos_test() ->
?assertEqual({error, 'invalid_will_qos', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 3:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_will_1_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 1:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 1:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_will_2_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 2:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 2:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_will_3_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 0:1, 1:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 0:1, 1:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_will_4_test() ->
?assertEqual({error, 'invalid_will_flgs', <<4:16, "MQTT", 4:8, 0:1, 1:1, 1:1, 2:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 1:1, 2:2, 0:1, 0:1, 0:1, 1:16>>))).

get_vh_invalid_pwd_flg_test() ->
?assertEqual({error, 'invalid_pwd_flg', <<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
	     (connect:get_vh(<<4:16, "MQTT", 4:8, 0:1, 1:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

%%%===================================================================
%%% Tests : get_proto
%%%===================================================================
get_proto_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto(<<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_proto_invalid_size_test() ->
    ?assertEqual({error, 'invalid_proto', <<5:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto(<<5:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_proto_invalid_case_1_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "Mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto(<<4:16, "Mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_proto_invalid_case_2_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto(<<4:16, "mqtt", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

get_proto_invalid_name_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto(<<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>))).

%%%===================================================================
%%% Tests : get_proto_lvl
%%%===================================================================

get_proto_lvl_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

get_proto_lvl_invalid_1_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 3:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 3:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

get_proto_lvl_invalid_2_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 5:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

get_proto_lvl_invalid_3_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 255:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 255:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

get_proto_lvl_invalid_4_test() ->
    ?assertEqual({error, 'connack1', <<4:16, "MQTT", 0:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}, 
		 (connect:get_proto_lvl({ok, <<4:16, "MQTT", 0:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).


get_proto_lvl_err_in_err_out_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_proto_lvl({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).


%%%===================================================================
%%% Tests : get_rsvd_flg
%%%===================================================================

get_rsvd_flg_valid_test() ->
    ?assertEqual({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

get_rsvd_flg_invalid_flg_test() ->
    ?assertEqual({error, 'invalid_rsvd_flg', <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 1:1, 1:16>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 1:1, 1:16>>}))).

get_rsvd_flg_no_conn_flg_test() ->
    ?assertEqual({error, 'no_conn_flgs', <<4:16, "MQTT", 4:8>>},
		 (connect:get_reserved_flg({ok, <<4:16, "MQTT", 4:8>>}))).

get_rsvd_flg_err_in_err_out_test() ->
    ?assertEqual({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>},
		 (connect:get_reserved_flg({error, 'invalid_proto', <<4:16, "foob", 4:8, 0:1, 0:1, 0:1, 0:2, 0:1, 0:1, 0:1, 1:16>>}))).

