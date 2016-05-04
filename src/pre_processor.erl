%%%-------------------------------------------------------------------
%%% @author kuldeep 
%%% @copyright (C) 2016, kuldeep
%%% @doc 
%%%
%%% @end Created : 13 April 2016 by kuldeep
%%% -------------------------------------------------------------------
-module(pre_processor).
-include("../include/yams_lib.hrl").
%% API
-export([interprete_fixed_header/1]).
%% Macro
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
-spec interprete_fixed_header(packet()) -> 
			         fixed_header_ok()
			          | fixed_header_error()
			          | type_byte_error().
				
interprete_fixed_header(Binary) ->
    interprete_remaining_length(interprete_type_byte(Binary)).
%%===================================================================
%% Roughly speaking, interprete_type_byte/1 behaves like an interpreter-front-end.
%% It scans (creates tokens from the) first byte of the control packet. 
%% And parses (analyzes) the tokens and determines their validity.
%% As an output the function returns following.
%%  1. Values of packet type and bit flags and remaining binary.
%%  2. Or error, its reason and the binary passed to it.
%%===================================================================
-type packet() :: <<_:16, _:_*8>>.
-type type_byte() :: #type_byte{}.

-type type_byte_ok() :: { 'ok'
			, type_byte()
			, binary()
			}.

-type type_byte_error() :: { 'error'
			   , 'invalid_fb'
			   , binary()
			   }.

-spec interprete_type_byte(packet()) -> 
				  type_byte_ok()
			        | type_byte_error().

interprete_type_byte(<<1:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = connect, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<2:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = connack, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<3:4, Dup:1, 0:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = publish, dup = Dup, qos = 0, retain = Retain}, RemainingBin};

interprete_type_byte(<<3:4, Dup:1, 1:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = publish, dup = Dup, qos = 1, retain = Retain}, RemainingBin};

interprete_type_byte(<<3:4, Dup:1, 2:2, Retain:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = publish, dup = Dup, qos = 2, retain = Retain}, RemainingBin};

interprete_type_byte(<<4:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = puback, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<5:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = pubrec, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<6:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = pubrel, dup = 0, qos = 1, retain = 0}, RemainingBin};

interprete_type_byte(<<7:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = pubcomp, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<8:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = subscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};

interprete_type_byte(<<9:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = suback, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<10:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = unsubscribe, dup = 0, qos = 1, retain = 0}, RemainingBin};

interprete_type_byte(<<11:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = unsuback, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<12:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = pingreq, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<13:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = pingresp, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(<<14:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    {ok, #type_byte{pkttype = disconnect, dup = 0, qos = 0, retain = 0}, RemainingBin};

interprete_type_byte(OtherBin) ->
    {error, invalid_tb, OtherBin}.
    
%%===================================================================
%% This function splits the remaining binary into...
%% 1. Remaining length of the packet, which I call it - var_load
%% 2. var_load (= variable header + payload)
%%===================================================================
-spec interprete_remaining_length(type_byte_ok()
			      |type_byte_error())->
				      fixed_header_ok() 
				     |fixed_header_error()
				     |type_byte_error().

interprete_remaining_length({ok, PacketType, RemainingBin}) ->
    interprete_remaining_length({PacketType, RemainingBin}, 0, 1);

interprete_remaining_length({error, invalid_tb, OtherBin}) ->
    {error, invalid_tb, OtherBin}.

%%==============================================
%% internal function.
%%==============================================
-type remaining_length() :: 0..?MAX_LENGTH.
-type fixed_header_ok() :: { ok
			   , type_byte()
			   , remaining_length()
			   , binary()}.
-type fixed_header_error() :: { error
			      , 'remaining_length_exceeds_max_length'
			      , { type_byte()
				, binary()
				}
			      }
			     |{ error
			      , 'remaining_length_differs_actual_length'
			      , type_byte()
			      , remaining_length()
			      , binary()
			      }.

-spec interprete_remaining_length({type_byte(),binary()},non_neg_integer(),pos_integer()) -> 
				       fixed_header_ok()
				     | fixed_header_error().
				    
interprete_remaining_length(TypeInterpretedPacket, _RemainingLength, Multiplier)
  when (Multiplier > 128 * 128 * 128) ->
    {error, remaining_length_exceeds_max_length, TypeInterpretedPacket};

%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
interprete_remaining_length({PacketType, <<1:1, Len:7, Rest/binary>>}, RemainingLength, Multiplier) ->
    interprete_remaining_length({PacketType, Rest}, RemainingLength + Len * Multiplier, Multiplier * 128);

%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
interprete_remaining_length({PacketType, <<0:1, Len:7, VarLoad/binary>>}, RemainingLength, Multiplier) ->
    RemLen = RemainingLength + (Len * Multiplier),
    case (RemLen =:= size(VarLoad)) of
	true -> 
	    {ok, PacketType, RemLen, VarLoad};
	false ->
	    {error, remaining_length_differs_actual_length, PacketType, RemLen, VarLoad}
    end.
