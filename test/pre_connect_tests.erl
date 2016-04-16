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

%% INVALID_TYPE_0 Test
validate_first_byte_for_invalid_type_0_test() ->
    ?assertEqual({error, invalid_fb, <<>>}, pre_connect:validate_first_byte(<<0:4, 0:1, 0:2, 0:1, 100:8>>)).

%% INVALID_TYPE_15 Test
validate_first_byte_for_invalid_type_15_test() ->
    ?assertEqual({error, invalid_fb, <<>>}, pre_connect:validate_first_byte(<<15:4, 0:1, 0:2, 0:1, 100:8>>)).

