%%%-------------------------------------------------------------------
%%% @author Kuldeep <>
%%% @copyright (C) 2016, Kuldeep
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2016 by Kuldeep <>
%%%-------------------------------------------------------------------
-module(connect).

%%include
-include("../include/connect.hrl").


%% API
-export([get_vh/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-type varload() :: binary().
-type varhead() :: #var_head{}.
-type payload() :: binary().

-spec get_vh(varload()) ->
		    { ok, varhead(), payload()}
		  | { error, connect_ret(), binary()}.
		    

get_vh(Binary) ->
    get_usr_flgs(get_will_flgs(get_reserved_flg(get_proto_lvl(get_proto(Binary))))).

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ==========
%% get protocol name
%% ==========

%% Value of first 2 bytes must be equal to 4.
%% And value of next 4 bytes must be "MQTT"
get_proto(<<4:16, "MQTT", RemainingBin/binary>>) ->
    {ok, <<4:16, "MQTT", RemainingBin/binary>>};

%% Otherwise, return error.
get_proto(Bin) ->
    {error, 'invalid_proto', Bin}.

%% ==========
%% get protocol level
%% ==========

%% Value of the seventh byte must be 4
%% It represent protocol level.
get_proto_lvl({ok, <<4:16, "MQTT", 4:8, RemainingBin/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, RemainingBin/binary>>};

%% If protocol level is not equal to 4,
%% error should be returned in connack
%% , and connection should be closed.
get_proto_lvl({ok, <<4:16, "MQTT", _:8, _RemainingBin>>} = Bin) ->
    {error, 'connack1', Bin};

%% When error is received to the fn, error is returned
get_proto_lvl({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get reserved flag
%% ==========

%% reserved flag must be 0
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, Pload/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, Pload/binary>>};

%% When reserved flag is 1, return error
%% and disconnect client
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, _WQos:2, _Will:1, _ClnSess:1, 1:1, _Pload/binary>> = Bin}) ->
    {error, 'invalid_rsvd_flg', Bin};

%% Its possible that no byte is present to represent conn flags.
%% return error
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, _RemainingBin/binary>> = Bin}) ->
    {error, 'no_conn_flgs', Bin};

%% When error is received to the fn, error is returned
get_reserved_flg({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get will flags
%% ==========

%% WQos = 3 is invalid.
get_will_flgs({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, 3:2, _Will:1, _ClnSess:1, _Rsvd:1, _Pload/binary>> = Bin}) ->
    {error, 'invalid_will_qos', Bin};

%% When Will=1, WQos(0/1) and WRtn(0/1/2) can have any values.
get_will_flgs({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, 1:1, ClnSess:1, 0:1, Pload/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, 1:1, ClnSess:1, 0:1, Pload/binary>>};

%% When Will=0, WQos and WRtn must be 0
get_will_flgs({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, 0:1, 0:2, 0:1, ClnSess:1, 0:1, Pload/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, 0:1, 0:2, 0:1, ClnSess:1, 0:1, Pload/binary>>};

%% When Will=0, WQos and WRtn must be 0
get_will_flgs({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, _WQos:2, 0:1, _ClnSess:1, 0:1, _Pload/binary>> = Bin}) ->
    {error, 'invalid_will', Bin};

%% When error is received to the fn, error is returned
get_will_flgs({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get user and password flags
%% ==========

%% When user name = 0, password must be 0
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, Pload/binary>>}) ->
    { ok
    , #var_head{ protocol = "MQTT", level = 4, connflgs = {0, 0, WRtn, WQos, Will, ClnSess, 0}}
    , Pload};

%% When user name = 0, password must not be 1
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 0:1, 1:1, _WRtn:1, _WQos:2, _Will:1, _ClnSess:1, 0:1, _Pload/binary>> = Bin}) ->
    { error
    , 'invalid_pwd_flg'
    , Bin};

%% When user name = 1, password can be 0 or 1
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 1:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, Pload/binary>>}) ->
    { ok
    , #var_head{ protocol = "MQTT", level = 4, connflgs = {1, Pwd, WRtn, WQos, Will, ClnSess, 0}}
    , Pload};

%% When error is received to the fn, error is returned
get_usr_flgs({error, _Reason, _Bin} = Error) ->
    Error.
