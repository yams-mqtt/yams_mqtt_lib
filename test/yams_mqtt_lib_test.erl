%%%-------------------------------------------------------------------
%%% @author Kuldeep <kuldeep@ThinkErl>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 21 May 2016 by Kuldeep <kuldeep@ThinkErl>
%%%-------------------------------------------------------------------
-module(yams_mqtt_lib_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests : get_vh
%%%===================================================================
get_len_valid_1_test() ->
    ?assertEqual({ok, 3, <<"abc">>}, yams_mqtt_lib:get_len(<<3:16, "abc">>)).

get_len_valid_2_test() ->
    ?assertEqual({ok, 3, <<"abc", 1:16, "a">>}, yams_mqtt_lib:get_len(<<3:16, "abc", 1:16, "a">>)).

get_len_invalid_test() ->
    ?assertEqual({error, invalid_input, <<3:8>>}, yams_mqtt_lib:get_len(<<3:8>>)).


decode_str_valid_1_test() ->
    ?assertEqual({ok, "abc", <<>>}, yams_mqtt_lib:decode_str(<<3:16, "abc">>)).

decode_str_valid_2_test() ->
    ?assertEqual({ok, "abc", <<1:16, "a">>}, yams_mqtt_lib:decode_str(<<3:16, "abc", 1:16, "a">>)).

decode_str_invalid_test() ->
    ?assertEqual({error, invalid_input, <<3:8>>}, yams_mqtt_lib:decode_str(<<3:8>>)).
