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

-record(testdata, {msgtype, dup, qos, retain}).
%%%===================================================================
%%% Tests
%%%===================================================================

%% CONNECT Test
get_msg_type_for_connect_test() ->
    Bin = <<1:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, connect, Bin}, pre_connect:get_msg_type(Bin)).

%% CONNACK Test
get_msg_type_for_connack_test() ->
    Bin = <<2:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, connack, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBLISH Test
get_msg_type_for_publish_test() ->
    Bin = <<3:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, publish, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBLISH Test with DUP flag = 1
get_msg_type_for_publish_with_dup_flag_value_1_test() ->
    Bin = <<3:4, 1:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, publish, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBLISH Test with Qos flag = 1
get_msg_type_for_publish_with_qos_flag_value_1_test() ->
    Bin = <<3:4, 1:1, 1:2, 0:1, 100:8>>,
    ?assertEqual({ok, publish, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBLISH Test with Qos flag = 2
get_msg_type_for_publish_with_qos_flag_value_2_test() ->
    Bin = <<3:4, 0:1, 2:2, 0:1, 100:8>>,
    ?assertEqual({ok, publish, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBLISH Test with Retain flag = 1
get_msg_type_for_publish_with_retain_flag_value_1_test() ->
    Bin = <<3:4, 0:1, 2:2, 1:1, 100:8>>,
    ?assertEqual({ok, publish, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBACK Test
get_msg_type_for_puback_test() ->
    Bin = <<4:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, puback, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBREC Test
get_msg_type_for_pubrec_test() ->
    Bin = <<5:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, pubrec, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBREL Test
get_msg_type_for_pubrel_test() ->
    Bin = <<6:4, 0:1, 1:2, 0:1, 100:8>>,
    ?assertEqual({ok, pubrel, Bin}, pre_connect:get_msg_type(Bin)).

%% PUBCOMP Test
get_msg_type_for_pubcomp_test() ->
    Bin = <<7:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, pubcomp, Bin}, pre_connect:get_msg_type(Bin)).

%% SUBSCRIBE Test
get_msg_type_for_subscribe_test() ->
    Bin = <<8:4, 0:1, 1:2, 0:1, 100:8>>,
    ?assertEqual({ok, subscribe, Bin}, pre_connect:get_msg_type(Bin)).

%% SUBACK Test
get_msg_type_for_suback_test() ->
    Bin = <<9:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, suback, Bin}, pre_connect:get_msg_type(Bin)).

%% UNSUBSCRIBE Test
get_msg_type_for_unsubscribe_test() ->
    Bin = <<10:4, 0:1, 1:2, 0:1, 100:8>>,
    ?assertEqual({ok, unsubscribe, Bin}, pre_connect:get_msg_type(Bin)).

%% UNSUBACK Test
get_msg_type_for_unsuback_test() ->
    Bin = <<11:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, unsuback, Bin}, pre_connect:get_msg_type(Bin)).

%% PINGREQ Test
get_msg_type_for_pingreq_test() ->
    Bin = <<12:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, pingreq, Bin}, pre_connect:get_msg_type(Bin)).

%% PINGRESP Test
get_msg_type_for_pingresp_test() ->
    Bin = <<13:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, pingresp, Bin}, pre_connect:get_msg_type(Bin)).

%% DISCONNECT Test
get_msg_type_for_disconnect_test() ->
    Bin = <<14:4, 0:1, 0:2, 0:1, 100:8>>,
    ?assertEqual({ok, disconnect, Bin}, pre_connect:get_msg_type(Bin)).

%% Test Invalid values
invalidate_msg_type_test() ->
    TstData = [ %%% Invalid Type = 0
		#testdata{msgtype = 0, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 0, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 0, dup = 0, qos = 0, retain = 1}
                %%% Invalid Type = 15
		#testdata{msgtype = 15, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 15, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 15, dup = 0, qos = 0, retain = 1}
                %%% Type = 1 (CONNECT)
		#testdata{msgtype = 1, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 1, dup = 0, qos = 0, retain = 1}
                %%% Type = 2 (CONNACK)
		#testdata{msgtype = 2, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 2, dup = 0, qos = 0, retain = 1}
                %%% Type = 3 (PUBLISH)
	        %%% , #testdata{msgtype = 3, dup = 0, qos = 3, retain = 0}
                %%% Type = 4 (PUBACK)
		#testdata{msgtype = 4, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 4, dup = 0, qos = 0, retain = 1}
                %%% Type = 5 (PUBREC)
		#testdata{msgtype = 5, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 5, dup = 0, qos = 0, retain = 1}
                %%% Type = 6 (PUBREL)
		#testdata{msgtype = 6, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 6, dup = 0, qos = 0, retain = 1}
                %%% Type = 7 (PUBCOMP)
		#testdata{msgtype = 7, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 7, dup = 0, qos = 0, retain = 1}
                %%% Type = 8 (SUBSCRIBE)
		#testdata{msgtype = 8, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 8, dup = 0, qos = 0, retain = 1}
                %%% Type = 9 (SUBACK)
		#testdata{msgtype = 9, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 9, dup = 0, qos = 0, retain = 1}
                %%% Type = 10 (UNSUBSCRIBE)
		#testdata{msgtype = 10, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 0, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 10, dup = 0, qos = 0, retain = 1}
                %%% Type = 11 (UNSUBACK)
		#testdata{msgtype = 11, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 11, dup = 0, qos = 0, retain = 1}
                %%% Type = 12 (PINGREQ)
		#testdata{msgtype = 12, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 12, dup = 0, qos = 0, retain = 1}
                %%% Type = 13 (PINGRESP)
		#testdata{msgtype = 13, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 13, dup = 0, qos = 0, retain = 1}
                %%% Type = 14 (DISCONNECT)
		#testdata{msgtype = 14, dup = 1, qos = 0, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 1, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 2, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 3, retain = 0}
	      , #testdata{msgtype = 14, dup = 0, qos = 0, retain = 1}
	      ],
    [assert_for_invalid_msg_type(TD) || TD <- TstData ].

assert_for_invalid_msg_type(#testdata{msgtype = MsgType, dup = Dup, qos = QoS, retain = Retain}) ->
    Bin = <<MsgType:4, Dup:1, QoS:2, Retain:1, 100:8>>,
    ?assertEqual({error, invalid_fb, Bin}, pre_connect:get_msg_type(Bin)).
