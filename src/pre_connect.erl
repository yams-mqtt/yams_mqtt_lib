%%%-------------------------------------------------------------------
%%% @author kuldeep 
%%% @copyright (C) 2016, kuldeep
%%% @doc
%%%
%%% @end Created : 13 April 2016 by kuldeep
%%% -------------------------------------------------------------------
-module(pre_connect).

%% API
-export([
	 validate_msg_type/1,
	 validate_remaining_length/1
	]).
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
%%Validate first byte of the packet to determine its type and validate
%%bit flags (bit#3 to bit#0)
validate_msg_type(<<1:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, connect, Rest};
validate_msg_type(<<2:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, connack, Rest};
validate_msg_type(<<3:4, _Dup:1, _QoS:2, _Retain:1, Rest/binary>>) -> 
    {ok, publish, Rest};
validate_msg_type(<<4:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, puback, Rest};
validate_msg_type(<<5:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, pubrec, Rest};
validate_msg_type(<<6:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
    {ok, pubrel, Rest};
validate_msg_type(<<7:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok,pubcomp, Rest};
validate_msg_type(<<8:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
    {ok, subscribe, Rest};
validate_msg_type(<<9:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, suback, Rest};
validate_msg_type(<<10:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
    {ok, unsubscribe, Rest};
validate_msg_type(<<11:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, unsuback, Rest};
validate_msg_type(<<12:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, pingreq, Rest};
validate_msg_type(<<13:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, pingresp, Rest};
validate_msg_type(<<14:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, disconnect, Rest};
validate_msg_type(_) -> 
    {error, invalid_fb, <<>>}.
    
%%===================================================================
%% Decode remaining length from the RestBin (RestBin does not contain
%% FirstByte). If value of the remaining length field is correct,
%% return rest of the binary.  Rest of the binary returned will
%% contain variable header and payload.  Rest of the binary returned
%% will not contain fixed header.  If value of the remaining length
%% field is not correct, return error.
validate_remaining_length(Rest) ->
    validate_remaining_length(Rest, 0, 1).

validate_remaining_length(_, RLength, _)
  when (RLength > ?MAX_LENGTH) ->
    {error, remaining_length_exceeds_max_length};
%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
validate_remaining_length(<<1:1, Len:7, Rest/binary>>, RLength, Multiplier) ->
    validate_remaining_length(Rest, RLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
validate_remaining_length(<<0:1, Len:7, Rest/binary>>, RLength, Multiplier)
  when ((RLength + Len * Multiplier) =:= size(Rest)) ->
    {ok, Rest};
%% Rest of the message is having invalid lenght.
validate_remaining_length(_, _, _) ->
    {error, invalid_rl}.
