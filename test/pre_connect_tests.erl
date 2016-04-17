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
validate_first_byte_for_connect_test() ->
    ?assertEqual({ok, connect, <<100>>}, pre_connect:validate_first_byte(<<1:4, 0:1, 0:2, 0:1, 100:8>>)).

%% CONNACK Test
validate_first_byte_for_connack_test() ->
    ?assertEqual({ok, connack, <<100>>}, pre_connect:validate_first_byte(<<2:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PUBLISH Test
validate_first_byte_for_publish_test() ->
    ?assertEqual({ok, publish, <<100>>}, pre_connect:validate_first_byte(<<3:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PUBLISH Test with DUP flag = 1
validate_first_byte_for_publish_with_dup_flag_value_1_test() ->
    ?assertEqual({ok, publish, <<100>>}, pre_connect:validate_first_byte(<<3:4, 1:1, 0:2, 0:1, 100:8>>)).

%% PUBLISH Test with Qos flag = 1
validate_first_byte_for_publish_with_qos_flag_value_1_test() ->
    ?assertEqual({ok, publish, <<100>>}, pre_connect:validate_first_byte(<<3:4, 1:1, 1:2, 0:1, 100:8>>)).

%% PUBLISH Test with Qos flag = 2
validate_first_byte_for_publish_with_qos_flag_value_2_test() ->
    ?assertEqual({ok, publish, <<100>>}, pre_connect:validate_first_byte(<<3:4, 0:1, 2:2, 0:1, 100:8>>)).

%% PUBLISH Test with Retain flag = 1
validate_first_byte_for_publish_with_retain_flag_value_1_test() ->
    ?assertEqual({ok, publish, <<100>>}, pre_connect:validate_first_byte(<<3:4, 0:1, 2:2, 1:1, 100:8>>)).

%% PUBACK Test
validate_first_byte_for_puback_test() ->
    ?assertEqual({ok, puback, <<100>>}, pre_connect:validate_first_byte(<<4:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PUBREC Test
validate_first_byte_for_pubrec_test() ->
    ?assertEqual({ok, pubrec, <<100>>}, pre_connect:validate_first_byte(<<5:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PUBREL Test
validate_first_byte_for_pubrel_test() ->
    ?assertEqual({ok, pubrel, <<100>>}, pre_connect:validate_first_byte(<<6:4, 0:1, 1:2, 0:1, 100:8>>)).

%% PUBCOMP Test
validate_first_byte_for_pubcomp_test() ->
    ?assertEqual({ok, pubcomp, <<100>>}, pre_connect:validate_first_byte(<<7:4, 0:1, 0:2, 0:1, 100:8>>)).

%% SUBSCRIBE Test
validate_first_byte_for_subscribe_test() ->
    ?assertEqual({ok, subscribe, <<100>>}, pre_connect:validate_first_byte(<<8:4, 0:1, 1:2, 0:1, 100:8>>)).

%% SUBACK Test
validate_first_byte_for_suback_test() ->
    ?assertEqual({ok, suback, <<100>>}, pre_connect:validate_first_byte(<<9:4, 0:1, 0:2, 0:1, 100:8>>)).

%% UNSUBSCRIBE Test
validate_first_byte_for_unsubscribe_test() ->
    ?assertEqual({ok, unsubscribe, <<100>>}, pre_connect:validate_first_byte(<<10:4, 0:1, 1:2, 0:1, 100:8>>)).

%% UNSUBACK Test
validate_first_byte_for_unsuback_test() ->
    ?assertEqual({ok, unsuback, <<100>>}, pre_connect:validate_first_byte(<<11:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PINGREQ Test
validate_first_byte_for_pingreq_test() ->
    ?assertEqual({ok, pingreq, <<100>>}, pre_connect:validate_first_byte(<<12:4, 0:1, 0:2, 0:1, 100:8>>)).

%% PINGRESP Test
validate_first_byte_for_pingresp_test() ->
    ?assertEqual({ok, pingresp, <<100>>}, pre_connect:validate_first_byte(<<13:4, 0:1, 0:2, 0:1, 100:8>>)).

%% DISCONNECT Test
validate_first_byte_for_disconnect_test() ->
    ?assertEqual({ok, disconnect, <<100>>}, pre_connect:validate_first_byte(<<14:4, 0:1, 0:2, 0:1, 100:8>>)).

%% Test Invalid values
invalidate_first_byte_test() ->
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
		%%% #testdata{msgtype = 3, dup = 1, qos = 0, retain = 0}
                %%% , #testdata{msgtype = 3, dup = 0, qos = 1, retain = 0}
	        %%% , #testdata{msgtype = 3, dup = 0, qos = 2, retain = 0}
	        %%% , #testdata{msgtype = 3, dup = 0, qos = 3, retain = 0}
	        %%% , #testdata{msgtype = 3, dup = 0, qos = 0, retain = 1}
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
    [assert_for_invalid_first_byte(TD) || TD <- TstData ].

assert_for_invalid_first_byte(#testdata{msgtype = MsgType, dup = Dup, qos = QoS, retain = Retain}) ->
    ?assertEqual({error, invalid_fb, <<>>}, pre_connect:validate_first_byte(<<MsgType:4, Dup:1, QoS:2, Retain:1, 100:8>>)).
