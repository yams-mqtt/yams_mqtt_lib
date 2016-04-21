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
	 get_msg_type/1,
	 validate_remaining_length/1
	]).
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
%% Validate first byte of the packet as follows...
%% Validate and determine message type. 
%% Validate bit flags (bit#3 to bit#0).
get_msg_type(<<1:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, connect, Bin};
get_msg_type(<<2:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, connack, Bin};
get_msg_type(<<3:4, _Dup:1, 0:2, _Retain:1, _RemainingBin>> = Bin) -> 
    {ok, publish, Bin};
get_msg_type(<<3:4, _Dup:1, 1:2, _Retain:1, _RemainingBin>> = Bin) -> 
    {ok, publish, Bin};
get_msg_type(<<3:4, _Dup:1, 2:2, _Retain:1, _RemainingBin>> = Bin) -> 
    {ok, publish, Bin};
get_msg_type(<<4:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, puback, Bin};
get_msg_type(<<5:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, pubrec, Bin};
get_msg_type(<<6:4, 0:1, 1:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, pubrel, Bin};
get_msg_type(<<7:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok,pubcomp, Bin};
get_msg_type(<<8:4, 0:1, 1:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, subscribe, Bin};
get_msg_type(<<9:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, suback, Bin};
get_msg_type(<<10:4, 0:1, 1:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, unsubscribe, Bin};
get_msg_type(<<11:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, unsuback, Bin};
get_msg_type(<<12:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, pingreq, Bin};
get_msg_type(<<13:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, pingresp, Bin};
get_msg_type(<<14:4, 0:1, 0:2, 0:1, _RemainingBin>> = Bin) -> 
    {ok, disconnect, Bin};
get_msg_type(_Bin) -> 
    {error, invalid_fb, _Bin}.
    
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
