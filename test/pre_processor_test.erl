%%%-------------------------------------------------------------------
%%% @author Kuldeep <kuldeep@ThinkErl>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Kuldeep <kuldeep@ThinkErl>
%%%-------------------------------------------------------------------
-module(pre_processor_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/yams_lib.hrl").
-record(testdata, {pkttype, dup, qos, retain}).
%%%===================================================================
%%% Tests
%%%===================================================================

%% CONNECT Test
get_msg_type_for_connect_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup = 0, qos = 0, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<1:4, 0:1, 0:2, 0:1, 100:8>>))).

%% CONNACK Test
get_msg_type_for_connack_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connack, dup = 0, qos = 0, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<2:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBLISH Test
get_msg_type_for_publish_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup = 0, qos = 0, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<3:4, 0:1, 0:2, 0:1, 100:8>>))).

%% PUBLISH Test with DUP flag = 1
get_msg_type_for_publish_with_dup_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup = 1, qos = 0, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<3:4, 1:1, 0:2, 0:1, 100:8>>))).

%% PUBLISH Test with Qos flag = 1
get_msg_type_for_publish_with_qos_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup = 1, qos = 1, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<3:4, 1:1, 1:2, 0:1, 100:8>>))).

%% PUBLISH Test with Qos flag = 2
get_msg_type_for_publish_with_qos_flag_value_2_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup = 0, qos = 2, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<3:4, 0:1, 2:2, 0:1, 100:8>>))).

%% PUBLISH Test with Retain flag = 1
get_msg_type_for_publish_with_retain_flag_value_1_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup = 0, qos = 2, retain = 1}, <<100>>},
		  (pre_processor:interprete_type_byte(<<3:4, 0:1, 2:2, 1:1, 100:8>>))).

%% PUBACK Test
get_msg_type_for_puback_test() ->
    ?assertEqual({ok, #type_byte{pkttype = puback, dup = 0, qos = 0, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<4:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBREC Test
get_msg_type_for_pubrec_test() ->
    ?assertEqual({ok, #type_byte{pkttype = pubrec, dup = 0, qos = 0, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<5:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PUBREL Test
get_msg_type_for_pubrel_test() ->
    ?assertEqual({ok, #type_byte{pkttype = pubrel, dup = 0, qos = 1, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<6:4, 0:1, 1:2, 0:1, 2:8>>))).

%% PUBCOMP Test
get_msg_type_for_pubcomp_test() ->
    ?assertEqual({ok, #type_byte{pkttype = pubcomp, dup = 0, qos = 0, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<7:4, 0:1, 0:2, 0:1, 2:8>>))).

%% SUBSCRIBE Test
get_msg_type_for_subscribe_test() ->
    ?assertEqual({ok, #type_byte{pkttype = subscribe, dup = 0, qos = 1, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<8:4, 0:1, 1:2, 0:1, 100:8>>))).

%% SUBACK Test
get_msg_type_for_suback_test() ->
    ?assertEqual({ok, #type_byte{pkttype = suback, dup = 0, qos = 0, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<9:4, 0:1, 0:2, 0:1, 100:8>>))).

%% UNSUBSCRIBE Test
get_msg_type_for_unsubscribe_test() ->
    ?assertEqual({ok, #type_byte{pkttype = unsubscribe, dup = 0, qos = 1, retain = 0}, <<100>>},
		  (pre_processor:interprete_type_byte(<<10:4, 0:1, 1:2, 0:1, 100:8>>))).

%% UNSUBACK Test
get_msg_type_for_unsuback_test() ->
    ?assertEqual({ok, #type_byte{pkttype = unsuback, dup = 0, qos = 0, retain = 0}, <<2>>},
		  (pre_processor:interprete_type_byte(<<11:4, 0:1, 0:2, 0:1, 2:8>>))).

%% PINGREQ Test
get_msg_type_for_pingreq_test() ->
    ?assertEqual({ok, #type_byte{pkttype = pingreq, dup = 0, qos = 0, retain = 0}, <<0>>},
		  (pre_processor:interprete_type_byte(<<12:4, 0:1, 0:2, 0:1, 0:8>>))).

%% pingresp Test
get_msg_type_for_pingresp_test() ->
    ?assertEqual({ok, #type_byte{pkttype = pingresp, dup = 0, qos = 0, retain = 0}, <<0>>},
		  (pre_processor:interprete_type_byte(<<13:4, 0:1, 0:2, 0:1, 0:8>>))).

%% DISCONNECT Test
get_msg_type_for_disconnect_test() ->
    ?assertEqual({ok, #type_byte{pkttype = disconnect, dup = 0, qos = 0, retain = 0}, <<0>>},
		  (pre_processor:interprete_type_byte(<<14:4, 0:1, 0:2, 0:1, 0:8>>))).

%% Test Invalid values
get_test_data() ->
             [ %%% Type = 0
		#testdata{pkttype = 0, dup = 0, qos = 0, retain = 0}
	      , #testdata{pkttype = 0, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 0, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 0, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 0, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 0, dup = 0, qos = 0, retain = 1}
                %%% Invalid Type = 15
	      , #testdata{pkttype = 15, dup = 0, qos = 0, retain = 0}
	      , #testdata{pkttype = 15, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 15, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 15, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 15, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 15, dup = 0, qos = 0, retain = 1}
                %%% Type = 1 (CONNECT)
	      , #testdata{pkttype = 1, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 1, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 1, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 1, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 1, dup = 0, qos = 0, retain = 1}
                %%% Type = 2 (CONNACK)
	      , #testdata{pkttype = 2, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 2, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 2, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 2, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 2, dup = 0, qos = 0, retain = 1}
                %%% Type = 3 (PUBLISH)
	      , #testdata{pkttype = 3, dup = 0, qos = 3, retain = 0}
                %%% Type = 4 (PUBACK)
	      , #testdata{pkttype = 4, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 4, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 4, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 4, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 4, dup = 0, qos = 0, retain = 1}
                %%% Type = 5 (PUBREC)
	      , #testdata{pkttype = 5, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 5, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 5, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 5, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 5, dup = 0, qos = 0, retain = 1}
                %%% Type = 6 (PUBREL)
	      , #testdata{pkttype = 6, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 6, dup = 0, qos = 0, retain = 0}
	      , #testdata{pkttype = 6, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 6, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 6, dup = 0, qos = 0, retain = 1}
                %%% Type = 7 (PUBCOMP)
	      , #testdata{pkttype = 7, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 7, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 7, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 7, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 7, dup = 0, qos = 0, retain = 1}
                %%% Type = 8 (SUBSCRIBE)
	      , #testdata{pkttype = 8, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 8, dup = 0, qos = 0, retain = 0}
	      , #testdata{pkttype = 8, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 8, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 8, dup = 0, qos = 0, retain = 1}
                %%% Type = 9 (SUBACK)
	      , #testdata{pkttype = 9, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 9, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 9, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 9, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 9, dup = 0, qos = 0, retain = 1}
                %%% Type = 10 (UNSUBSCRIBE)
	      , #testdata{pkttype = 10, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 10, dup = 0, qos = 0, retain = 0}
	      , #testdata{pkttype = 10, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 10, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 10, dup = 0, qos = 0, retain = 1}
                %%% Type = 11 (UNSUBACK)
	      , #testdata{pkttype = 11, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 11, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 11, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 11, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 11, dup = 0, qos = 0, retain = 1}
                %%% Type = 12 (PINGREQ)
	      , #testdata{pkttype = 12, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 12, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 12, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 12, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 12, dup = 0, qos = 0, retain = 1}
                %%% Type = 13 (PINGRESP)
	      , #testdata{pkttype = 13, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 13, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 13, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 13, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 13, dup = 0, qos = 0, retain = 1}
                %%% Type = 14 (DISCONNECT)
	      , #testdata{pkttype = 14, dup = 1, qos = 0, retain = 0}
	      , #testdata{pkttype = 14, dup = 0, qos = 1, retain = 0}
	      , #testdata{pkttype = 14, dup = 0, qos = 2, retain = 0}
	      , #testdata{pkttype = 14, dup = 0, qos = 3, retain = 0}
	      , #testdata{pkttype = 14, dup = 0, qos = 0, retain = 1}
	      ].

invalid_type_byte_test_() ->
    TestData = get_test_data(),
    [?_assertEqual({error, invalid_fb, <<Pkttype:4, Dup:1, QoS:2, Retain:1, 100:8>>}, 
		  (pre_processor:interprete_type_byte(<<Pkttype:4, Dup:1, QoS:2, Retain:1, 100:8>>)))
     || #testdata{pkttype = Pkttype, dup = Dup, qos = QoS, retain = Retain} <- TestData ].

%%==========================================================================
%% Pass result of invalid packet type to the interprete_remaining_length function.
interprete_remaining_length_when_packetype_has_error_test_() ->
    TestData = get_test_data(),
    [?_assertEqual({error, invalid_fb, <<Pkttype:4, Dup:1, QoS:2, Retain:1, 100:8>>}, 
		  (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<Pkttype:4, Dup:1, QoS:2, Retain:1, 100:8>>))))
     || #testdata{pkttype = Pkttype, dup = Dup, qos = QoS, retain = Retain} <- TestData ].

%% interprete_remaining_length_test - when remaining binary is too short
interprete_remaining_length_when_remaining_binary_is_too_short_test() ->
    ?assertEqual({error, remaining_length_differs_actual_length, {type_byte,connect,0,0,0}, 1, <<>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 1:8>>)))).

%% interprete_remaining_length_test - when remaining binary is too long
interprete_remaining_length_when_remaining_binary_is_too_long_test() ->
    ?assertEqual({error, remaining_length_differs_actual_length, {type_byte,connect,0,0,0}, 1, <<100:16>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 1:8, 100:16>>)))).

%% interprete_remaining_length_test - lower limit of the first byte of the remaining length.
interprete_remaining_length_0_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 0, <<>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 0:8>>)))).

%% interprete_remaining_length_test - where remaining length is 2 bytes.
interprete_remaining_length_1_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 2, <<100:16>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 2:8, 100:16>>)))).
%% interprete_remaining_length_test - upper limit of the first byte of the remaining length.
interprete_remaining_length_127_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 127, <<100:1016>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 127:8, 100:1016>>)))).

%% interprete_remaining_length_test - lower limit of the second byte of the remaining length.
interprete_remaining_length_128_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 128, <<100:1024>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 128:8, 1:8, 100:1024>>)))).

%% interprete_remaining_length_test - upper limit of the second byte of the remaining length.
interprete_remaining_length_16383_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 16383, <<100:131064>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 255:8, 127:8, 100:131064>>)))).

%% interprete_remaining_length_test - lower limit of the third byte of the remaining length.
interprete_remaining_length_16384_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 16384, <<100:131072>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 128:8, 128:8, 1:8, 100:131072>>)))).


%% interprete_remaining_length_test - upper limit of the third byte of the remaining length.
interprete_remaining_length_2097151_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 2097151, <<100:16777208>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 255:8, 255:8, 127:8, 100:16777208>>)))).

%% interprete_remaining_length_test - lower limit of the fourth byte of the remaining length.
interprete_remaining_length_2097152_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 2097152, <<100:16777216>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 128:8, 128:8, 128:8, 1:8, 100:16777216>>)))).

%%=========================
%% Note : Following 2 tests are long running tests.
%% Running them locally, hangs local VM.
%% While they run OK on machines provision by TravisCI.
%%=========================

%% %% interprete_remaining_length_test - upper limit of the fourth byte of the remaining length.
interprete_remaining_length_268435455_test() ->
    ?assertEqual({ok, #type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, 268435455, <<100:2147483640>>},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 255:8, 255:8, 255:8, 127:8, 100:2147483640>>)))).

%% %% %% interprete_remaining_length_test - exceeding the upper limit of the fourth byte of the remaining length.
interprete_remaining_length_268435456_test() ->
    ?assertEqual({error, remaining_length_exceeds_max_length, {#type_byte{pkttype = connect, dup =0, qos = 0, retain = 0}, <<1:8, 100:2147483648>>}},
		 (pre_processor:interprete_remaining_length(pre_processor:interprete_type_byte(<<1:4, 0:4, 128:8, 128:8, 128:8, 128:8, 1:8, 100:2147483648>>)))).

%%=========================
%% interprete packet
%%=========================

%% interprete_remaining_length_test - lower limit of the second byte of the remaining length.
interprete_packet_test() ->
    ?assertEqual({ok, #type_byte{pkttype = publish, dup =0, qos = 0, retain = 0}, 128, <<100:1024>>},
		 (pre_processor:interprete_packet(<<3:4, 0:4, 128:8, 1:8, 100:1024>>))).
