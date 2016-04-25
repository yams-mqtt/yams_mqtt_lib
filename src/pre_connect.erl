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
	 compile_packet_type/1,
	 compile_remaining_length/1
	]).
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
%% Roughly speaking, compile_packet_type behaves like a compiler-front-end.
%% It scans (creates tokens from the) first byte of the control packet (a packet henceforth), 
%% And parses (analyzes) the tokens and determines if they are valid values or errors.
%% As an output the function returns...
%%     values of packet type and bit flags, extracted from the type-byte and remaining binary or
%%     error along with its reason and the binary passed to it.
compile_packet_type(<<1:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = connect, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<2:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = connack, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<3:4, Dup:1, 0:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = publish, dup = Dup, qos = 0, retain = Retain}, RemainingBin};
compile_packet_type(<<3:4, Dup:1, 1:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = publish, dup = Dup, qos = 1, retain = Retain}, RemainingBin};
compile_packet_type(<<3:4, Dup:1, 2:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = publish, dup = Dup, qos = 2, retain = Retain}, RemainingBin};
compile_packet_type(<<4:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = puback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<5:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = pubrec, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<6:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = pubrel, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_packet_type(<<7:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = pubcomp, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<8:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = subscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_packet_type(<<9:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = suback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<10:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = unsubscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};
compile_packet_type(<<11:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = unsuback, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<12:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = pingreq, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<13:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = pingresp, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(<<14:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #packet_type{msgtype = disconnect, dup = 0, qos = 0, retain = 0}, RemainingBin};
compile_packet_type(OtherBin) ->
    {error, invalid_fb, OtherBin}.
    
%%===================================================================
%% This function splits the remaining binary into...
%% 1. Remaining length of the packet, which I call it - var_load (= variable header + payload)
%% 2. var_load
compile_remaining_length({ok, PacketType, RemainingBin}) ->
    compile_remaining_length({PacketType, RemainingBin}, 0, 1);
compile_remaining_length({error, invalid_fb, OtherBin}) ->
    {error, invalid_fb, OtherBin};
compile_remaining_length(Input) ->
    {error, invalid_input, Input}.


compile_remaining_length(TypeCompiledPacket, RemainingLength, _Multiplier)
  when (RemainingLength > ?MAX_LENGTH) ->
    {error, remaining_length_exceeds_max_length, TypeCompiledPacket};
%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
compile_remaining_length({PacketType, <<1:1, Len:7, Rest/binary>>}, RemainingLength, Multiplier) ->
    compile_remaining_length({PacketType, Rest}, RemainingLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
compile_remaining_length({PacketType, <<0:1, Len:7, VarLoad/binary>>}, RemainingLength, Multiplier) ->
    RemLen = RemainingLength + (Len * Multiplier),
    case (RemLen =:= size(VarLoad)) of
	true -> 
	    {ok, PacketType, RemLen, VarLoad};
	false ->
	    {error, remaining_length_value_unequal_to_the_actual_length, PacketType, RemLen, VarLoad}
    end.
