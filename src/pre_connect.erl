%%%-------------------------------------------------------------------
%%% @author kuldeep 
%%% @copyright (C) 2016, kuldeep
%%% @doc
%%%
%%% @end Created : 13 April 2016 by kuldeep
%%% -------------------------------------------------------------------
-module(pre_connect).
-include("../include/yams_lib.hrl").
%% API
-export([
	 compile_type_byte/1,
	 get_var_load/1
	]).
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
%% Roughly speaking, compile_type_byte behaves like a compiler-front-end.
%% It scans (creates tokens from the) first byte of the control packet (a packet henceforth), 
%% And parses (analyzes) the tokens and determines if they are valid values or errors.
%% As an output the function returns...
%%     values of packet type and bit flags, extracted from the type-byte and remaining binary or
%%     error along with its reason and the binary passed to i.
compile_type_byte(<<1:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = connect, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<2:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = connack, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<3:4, Dup:1, 0:2, Retain:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = publish, dup = Dup, qos = 0, retain = Retain}, RemainingBin};
compile_type_byte(<<3:4, Dup:1, 1:2, Retain:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = publish, dup = Dup, qos = 1, retain = Retain}, RemainingBin};
compile_type_byte(<<3:4, Dup:1, 2:2, Retain:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = publish, dup = Dup, qos = 2, retain = Retain}, RemainingBin};
compile_type_byte(<<4:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = puback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<5:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = pubrec, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<6:4, 0:1, 1:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = pubrel, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_type_byte(<<7:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = pubcomp, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<8:4, 0:1, 1:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = subscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_type_byte(<<9:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = suback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<10:4, 0:1, 1:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = unsubscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_type_byte(<<11:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = unsuback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<12:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = pingreq, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<13:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = pingresp, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(<<14:4, 0:1, 0:2, 0:1, RemainingBin>>) ->
    {ok, #type_byte{msgtype = disconnect, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_type_byte(OtherBin) ->
    {error, invalid_fb, OtherBin}.
    
%%===================================================================
%% This function splits the binary into...
%% 1. First byte (message type byte), which is ignored by the function.
%% 2. Remaining length of the var_load (= variable header + payload)
%% 3. var_load
get_var_load(<<_TypeByte:8, RestBin/binary>>) ->
    get_var_load(RestBin, 0, 1).

get_var_load(_, RLength, _)
  when (RLength > ?MAX_LENGTH) ->
    {error, remaining_length_exceeds_max_length};
%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
get_var_load(<<1:1, Len:7, Rest/binary>>, RLength, Multiplier) ->
    get_var_load(Rest, RLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
get_var_load(<<0:1, Len:7, Rest/binary>>, RLength, Multiplier)
  when ((RLength + Len * Multiplier) =:= size(Rest)) ->
    {ok, Rest};
%% Rest of the message is having invalid length.
get_var_load(_, _, _) ->
    {error, invalid_rl}.
