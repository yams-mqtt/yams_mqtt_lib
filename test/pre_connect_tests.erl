%%%-------------------------------------------------------------------
%%% @author Kuldeep <kuldeep@ThinkErl>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Kuldeep <kuldeep@ThinkErl>
%%%-------------------------------------------------------------------
-module(pre_connect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/yams_lib.hrl").
-record(testdata, {msgtype, dup, qos, retain}).
%%%===================================================================
%%% Tests
%%%===================================================================

%% CONNECT Test
get_msg_type_for_connect_test() ->
    ?assertEqual({ok, #type_byte{msgtype = connect, dup = 0, qos = 0, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<1:4, 0:1, 0:2, 0:1, 100:8>>))).

%% CONNACK Test
get_msg_type_for_connack_test() ->
    ?assertEqual({ok, #type_byte{msgtype = connack, dup = 0, qos = 0, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<2:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBLISH Test
get_msg_type_for_publish_test() ->
    ?assertEqual({ok, #type_byte{msgtype = publish, dup = 0, qos = 0, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<3:4, 0:1, 0:2, 0:1, 100:8>>))).

%% PUBLISH Test with DUP flag = 1
get_msg_type_for_publish_with_dup_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{msgtype = publish, dup = 1, qos = 0, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<3:4, 1:1, 0:2, 0:1, 100:8>>))).

%% PUBLISH Test with Qos flag = 1
get_msg_type_for_publish_with_qos_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{msgtype = publish, dup = 1, qos = 1, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<3:4, 1:1, 1:2, 0:1, 100:8>>))).

%% PUBLISH Test with Qos flag = 2
get_msg_type_for_publish_with_qos_flag_value_2_test() ->
    ?assertEqual({ok, #type_byte{msgtype = publish, dup = 0, qos = 2, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<3:4, 0:1, 2:2, 0:1, 100:8>>))).

%% PUBLISH Test with Retain flag = 1
get_msg_type_for_publish_with_retain_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{msgtype = publish, dup = 0, qos = 2, retain = 1}, 100}
		,(pre_connect:compile_type_byte(<<3:4, 0:1, 2:2, 1:1, 100:8>>))).

%% PUBACK Test
get_msg_type_for_puback_test() ->
    ?assertEqual({ok, #type_byte{msgtype = puback, dup = 0, qos = 0, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<4:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBREC Test
get_msg_type_for_pubrec_test() ->
    ?assertEqual({ok, #type_byte{msgtype = pubrec, dup = 0, qos = 0, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<5:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBREL Test
get_msg_type_for_pubrel_test() ->
    ?assertEqual({ok, #type_byte{msgtype = pubrel, dup = 0, qos = 1, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<6:4, 0:1, 1:2, 0:1, 2:8>>))).

%% PUBCOMP Test
get_msg_type_for_pubcomp_test() ->
    ?assertEqual({ok, #type_byte{msgtype = pubcomp, dup = 0, qos = 0, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<7:4, 0:1, 0:2, 0:1, 2:8>>))).

%% SUBSCRIBE Test
get_msg_type_for_subscribe_test() ->
    ?assertEqual({ok, #type_byte{msgtype = subscribe, dup = 0, qos = 1, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<8:4, 0:1, 1:2, 0:1, 100:8>>))).

%% SUBACK Test
get_msg_type_for_suback_test() ->
    ?assertEqual({ok, #type_byte{msgtype = suback, dup = 0, qos = 0, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<9:4, 0:1, 0:2, 0:1, 100:8>>))).

%% UNSUBSCRIBE Test
get_msg_type_for_unsubscribe_test() ->
    ?assertEqual({ok, #type_byte{msgtype = unsubscribe, dup = 0, qos = 1, retain = 0}, 100}
		,(pre_connect:compile_type_byte(<<10:4, 0:1, 1:2, 0:1, 100:8>>))).

%% UNSUBACK Test
get_msg_type_for_unsuback_test() ->
    ?assertEqual({ok, #type_byte{msgtype = unsuback, dup = 0, qos = 0, retain = 0}, 2}
		,(pre_connect:compile_type_byte(<<11:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PINGREQ Test
get_msg_type_for_pingreq_test() ->
    ?assertEqual({ok, #type_byte{msgtype = pingreq, dup = 0, qos = 0, retain = 0}, 0}
		,(pre_connect:compile_type_byte(<<12:4, 0:1, 0:2, 0:1, 0:8>>))).

%% PINGRESP Test
get_msg_type_for_pingresp_test() ->
    ?assertEqual({ok, #type_byte{msgtype = pingresp, dup = 0, qos = 0, retain = 0}, 0}
		,(pre_connect:compile_type_byte(<<13:4, 0:1, 0:2, 0:1, 0:8>>))).

%% DISCONNECT Test
get_msg_type_for_disconnect_test() ->
    ?assertEqual({ok, #type_byte{msgtype = disconnect, dup = 0, qos = 0, retain = 0}, 0}
		,(pre_connect:compile_type_byte(<<14:4, 0:1, 0:2, 0:1, 0:8>>))).

%% Test Invalid values
invalidate_type_byte_test() ->
    TstData = [ %%% Invalid Type = 0
		#testdata{msgtype = 0, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 0, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 0, retain = 1}
                %%% Invalid Type = 15
	      , #testdata{msgtype = 15, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 15, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 0, retain = 1}
                %%% Type = 1 (CONNECT)
	      , #testdata{msgtype = 1, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 0, retain = 1}
                %%% Type = 2 (CONNACK)
	      , #testdata{msgtype = 2, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 0, retain = 1}
                %%% Type = 3 (PUBLISH)
	      , #testdata{msgtype = 3, dup = 0, qos = 3, retain = 0}
                %%% Type = 4 (PUBACK)
	      , #testdata{msgtype = 4, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 0, retain = 1}
                %%% Type = 5 (PUBREC)
	      , #testdata{msgtype = 5, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 0, retain = 1}
                %%% Type = 6 (PUBREL)
	      , #testdata{msgtype = 6, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 0, retain = 1}
                %%% Type = 7 (PUBCOMP)
	      , #testdata{msgtype = 7, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 0, retain = 1}
                %%% Type = 8 (SUBSCRIBE)
	      , #testdata{msgtype = 8, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 0, retain = 1}
                %%% Type = 9 (SUBACK)
	      , #testdata{msgtype = 9, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 0, retain = 1}
                %%% Type = 10 (UNSUBSCRIBE)
	      , #testdata{msgtype = 10, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 0, retain = 1}
                %%% Type = 11 (UNSUBACK)
	      , #testdata{msgtype = 11, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 0, retain = 1}
                %%% Type = 12 (PINGREQ)
	      , #testdata{msgtype = 12, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 0, retain = 1}
                %%% Type = 13 (PINGRESP)
	      , #testdata{msgtype = 13, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 0, retain = 1}
                %%% Type = 14 (DISCONNECT)
	      , #testdata{msgtype = 14, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 0, retain = 1}
	      ],
    [assert_for_invalid_type_byte(TD) || TD <- TstData ].

assert_for_invalid_type_byte(#testdata{msgtype = MsgType, dup = Dup, qos = QoS, retain = Retain}) ->
    Bin = <<MsgType:4, Dup:1, QoS:2, Retain:1, 100:8>>,
    ?assertEqual({error, invalid_fb, Bin}, (pre_connect:compile_type_byte(Bin))).
