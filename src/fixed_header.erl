%%%-------------------------------------------------------------------
%%% @author kuldeep 
%%% @copyright (C) 2016, kuldeep
%%% @doc 
%%%
%%% @end Created : 13 April 2016 by kuldeep
%%% -------------------------------------------------------------------
-module(fixed_header).
-include("../include/fixed_header.hrl").
%% API
-export([get_fh/1]). %% get fixed header
%% Macro
-define(MAX_LENGTH, 268435455). % Maximum allowed length of the topic.

%%===================================================================
-spec get_fh(packet()) -> 
		fixed_header_ok()  
	      | fixed_header_error()
	      | type_byte_error().
				
get_fh(Binary) ->
    get_rl(get_tb(Binary)).



%%==============================================
%% internal function.
%%==============================================

%% Roughly speaking, get_tb/1 behaves like an interpreter-front-end.
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

%% Get type byte.
%% The type byte contains, packet type and bit-flags.
-spec get_tb(packet()) ->  type_byte_ok()  | type_byte_error().

get_tb(<<1:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = connect, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<2:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = connack, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<3:4, Dup:1, 0:2, Retain:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = publish, dup = Dup, qos = 0, retain = Retain}
    , RemainingBin
    };

get_tb(<<3:4, Dup:1, 1:2, Retain:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = publish, dup = Dup, qos = 1, retain = Retain}
    , RemainingBin
    };

get_tb(<<3:4, Dup:1, 2:2, Retain:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = publish, dup = Dup, qos = 2, retain = Retain}
    , RemainingBin
    };

get_tb(<<4:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = puback, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<5:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = pubrec, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<6:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = pubrel, dup = 0, qos = 1, retain = 0}
    , RemainingBin
    };

get_tb(<<7:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = pubcomp, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<8:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = subscribe, dup = 0, qos = 1, retain = 0}
    , RemainingBin
    };

get_tb(<<9:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = suback, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<10:4, 0:1, 1:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = unsubscribe, dup = 0, qos = 1, retain = 0}
    , RemainingBin
    };

get_tb(<<11:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = unsuback, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<12:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = pingreq, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<13:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = pingresp, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(<<14:4, 0:1, 0:2, 0:1, RemainingBin/binary>>) ->
    { ok
    , #type_byte{pkttype = disconnect, dup = 0, qos = 0, retain = 0}
    , RemainingBin
    };

get_tb(OtherBin) ->
    { error
    , invalid_tb
    , OtherBin
    }.
    
%%===================================================================
%% Get remaining length.
%% This function splits the remaining binary into...
%% 1. Remaining length of the packet,
%% 2. var_load (= variable header + payload)
%%===================================================================
-spec get_rl(type_byte_ok()  | type_byte_error()) ->
		fixed_header_ok()
	      | fixed_header_error()
	      | type_byte_error().

get_rl({error, invalid_tb, OtherBin}) ->
    {error, invalid_tb, OtherBin};

get_rl({ok, PacketType, RemainingBin}) ->
    get_rl({PacketType, RemainingBin}, 0, 1).

%%===================================================================
-type remaining_length() :: 0..?MAX_LENGTH.

-type varload() :: binary().

-type fixed_header_ok() :: { ok
			   , type_byte()
			   , remaining_length()
			   , varload()}.
-type fixed_header_error() :: { error
			      , 'rl_gt_ml' %% greater than max length
			      , { type_byte(), binary()}
			      }
			     |{ error
			      , 'rl_ne_al' %% not equal to actual length
			      , type_byte()
			      , remaining_length()
			      , binary()
			      }.

-spec get_rl({type_byte(), binary()}
	    , non_neg_integer()
	    , pos_integer()
	    ) ->
		    fixed_header_ok()
		  | fixed_header_error().

get_rl(TypeInterpretedPacket, _RemainingLength, Multiplier)
  when (Multiplier > 128 * 128 * 128) ->
    %% remaining length greater than maximum length
    {error, rl_gt_ml, TypeInterpretedPacket};

%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
get_rl({PacketType, <<1:1, Len:7, Rest/binary>>}
      , RemainingLength
      , Multiplier
      ) ->
    get_rl({PacketType, Rest}
	  , RemainingLength + Len * Multiplier
	  , Multiplier * 128);

%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
get_rl({PacketType, <<0:1, Len:7, VarLoad/binary>>}
      , RemainingLength
      , Multiplier
      ) ->
    RemLen = RemainingLength + (Len * Multiplier),
    case (RemLen =:= size(VarLoad)) of
	true ->
	    {ok, PacketType, RemLen, VarLoad};
	false ->
	    %% remaining length not equal to actual length
	    {error, rl_ne_al, PacketType, RemLen, VarLoad}
    end.
