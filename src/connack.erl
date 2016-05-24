%%%-------------------------------------------------------------------
%%% @author Kuldeep <>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2016 by Kuldeep <>
%%%-------------------------------------------------------------------
-module(connack).

%% API
-export([get_pkt/1]).

%%%===================================================================
%%% API
%%%===================================================================
-type session_present()     :: 0..1.
-type return_code()         :: 0..5.
-type illegal_return_code() :: 6..255.
-type connack_err()         :: {error, 'illegal_return_code', illegal_return_code()}.
-spec get_pkt({session_present(), return_code()}) ->
	   <<_:32>> | connack_err().
%%--------------------------------------------------------------------
%% @doc [MQTT-3.2.2-4]
%%      If a server sends a CONNACK packet containing a non-zero return code
%%      it MUST set Session Present to 0 [MQTT-3.2.2-4].
%% @spec
%% @end
%%--------------------------------------------------------------------
get_pkt({SessionPresent, 0}) ->
    <<2:4, 0:4, 2:8, 0:7, SessionPresent:1, 0:8>>;
get_pkt({_SessionPresent, ReturnCode}) when ReturnCode >= 1, ReturnCode =< 5 ->
    <<2:4, 0:4, 2:8, 0:7, 0:1, ReturnCode:8>>;
get_pkt({_SessionPresent, ReturnCode}) ->
     {error, 'illegal_return_code', ReturnCode}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
