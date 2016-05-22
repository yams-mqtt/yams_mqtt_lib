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
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-type conn_vl_bin()  :: binary(). %% varload bin. (varload = varhead + payload)
-type conn_vh()      :: #conn_vh{}. %% variable header for connect
-type conn_pl_bin()  :: binary(). %% payload bin.
-type conn_pl()      :: #conn_pl{}. %% payload for connect

-spec get_vh(conn_vl_bin()) ->
		    { ok, conn_vh(), conn_pl_bin()}
		  | { error, connect_ret(), binary()}.

-spec get_pl({ok, connect_flgs(), conn_pl_bin()}) ->
		    {ok, connect_flgs(), <<>>, conn_pl()}
		   |{error, connect_ret(), connect_flgs(), binary()}. 
		    
-spec parse(binary()) ->
		   {ok, conn_vh(), conn_pl()}
		 | {error, connect_ret(), binary()}.
parse(Binary) ->
    case (get_vh(Binary)) of 
	{ok, #conn_vh{ connflgs = ConnFlgs} = ConnVh, Payload} ->
            case get_pl({ok, ConnFlgs, Payload}) of
                {ok, _ConnFlgs, <<>>, ConnPl} ->
                    {ok, ConnVh, ConnPl};
	        {error, Reason, _ConnFlags, Bin} -> 
                    {error, Reason, Bin}
            end;
        Error ->
             Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% get variable header
get_vh(Binary) ->
    get_kat(get_usr_flgs(get_will_flgs(get_reserved_flg(get_proto_lvl(get_proto(Binary)))))).

%% get payload (only after variable header is extracted successfully.)
get_pl({ok, ConnFlags, Payload}) -> 
    chk_empty_payload(get_pwd(get_usr(get_will_msg(get_will_topic(get_client_id({ok, ConnFlags, Payload})))))).

%% ==========
%% get protocol name
%% ==========

%% Value of first 2 bytes must be equal to 4.
%% And value of next 4 bytes must be "MQTT"
get_proto(<<4:16, "MQTT", Rbin/binary>>) ->
    {ok, <<4:16, "MQTT", Rbin/binary>>};
%% Otherwise, return error.
get_proto(Bin) ->
    {error, 'invalid_proto', Bin}.

%% ==========
%% get protocol level
%% ==========

%% Value of the seventh byte must be 4
%% It represent protocol level.
get_proto_lvl({ok, <<4:16, "MQTT", 4:8, Rbin/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Rbin/binary>>};
%% If protocol level is not equal to 4,
%% error should be returned in connack
%% , and connection should be closed.
get_proto_lvl({ok, <<4:16, "MQTT", _:8, _Rbin/binary>> = Bin}) ->
    {error, 'connack1', Bin};
%% When error is received to the fn, error is returned
get_proto_lvl({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get reserved flag
%% ==========

%% reserved flag must be 0
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>};
%% When reserved flag is 1, return error
%% and disconnect client
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, _WQos:2, _Will:1, _ClnSess:1, 1:1, _RBin/binary>> = Bin}) ->
    {error, 'invalid_rsvd_flg', Bin};
%% Its possible that no byte is present to represent conn flags.
%% return error
get_reserved_flg({ok, <<4:16, "MQTT", 4:8, _Rbin/binary>> = Bin}) ->
    {error, 'no_conn_flgs', Bin};
%% When error is received to the fn, error is returned
get_reserved_flg({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get will flags
%% ==========

%% WQos = 3 is invalid.
get_will_flgs({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, 3:2, _Will:1, _ClnSess:1, _Rsvd:1, _RBin/binary>> = Bin}) ->
    {error, 'invalid_will_qos', Bin};
%% When Will=1, WQos(0/1) and WRtn(0/1/2) can have any values.
get_will_flgs({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, 1:1, ClnSess:1, 0:1, RBin/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, 1:1, ClnSess:1, 0:1, RBin/binary>>};
%% When Will=0, WQos and WRtn must be 0
get_will_flgs({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, 0:1, 0:2, 0:1, ClnSess:1, 0:1, RBin/binary>>}) ->
    {ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, 0:1, 0:2, 0:1, ClnSess:1, 0:1, RBin/binary>>};
%% When Will=0, WQos and WRtn must be 0
get_will_flgs({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, _WQos:2, 0:1, _ClnSess:1, 0:1, _RBin/binary>> = Bin}) ->
    {error, 'invalid_will_flgs', Bin};
%% When error is received to the fn, error is returned
get_will_flgs({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get user and password flags
%% ==========

%% When user name = 0, password must be 0
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>}) ->
    { ok, <<4:16, "MQTT", 4:8, 0:1, 0:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>};
%% When user name = 0, password must not be 1
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 0:1, 1:1, _WRtn:1, _WQos:2, _Will:1, _ClnSess:1, 0:1, _RBin/binary>> = Bin}) ->
    { error, 'invalid_pwd_flg', Bin};
%% When user name = 1, password can be 0 or 1
get_usr_flgs({ok, <<4:16, "MQTT", 4:8, 1:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>}) ->
    { ok, <<4:16, "MQTT", 4:8, 1:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, RBin/binary>>};
%% When error is received to the fn, error is returned
get_usr_flgs({error, _Reason, _Bin} = Error) ->
    Error.

%% ==========
%% get keep alive time (Kat)
%% ==========

%% Next 2 bytes after conn-flgs represent Keep alive time.
get_kat({ok, <<4:16, "MQTT", 4:8, Usr:1, Pwd:1, WRtn:1, WQos:2, Will:1, ClnSess:1, 0:1, Kat:16, Payload/binary>>}) ->
    {ok, #conn_vh{ protocol = "MQTT", level = 4, connflgs = {Usr, Pwd, WRtn, WQos, Will, ClnSess, 0}, kat = Kat}, Payload};
%% When Binary is not long enough to accomodate Kat, error is returned
get_kat({ok, <<4:16, "MQTT", 4:8, _Usr:1, _Pwd:1, _WRtn:1, _WQos:2, _Will:1, _ClnSess:1, 0:1, _RBin/binary>> = Bin}) ->
    {error, 'invalid_kat', Bin};
%% When error is received by the fn, error is returned
get_kat(Error) ->
    Error.


%% ==========
%% get client id
%% ==========
get_client_id({ok, ConnFlags, Payload}) ->
    case yams_mqtt_lib:decode_str(Payload) of
	{ok, Str, RBin} ->
	    {ok, ConnFlags, RBin, #conn_pl{ id = Str }};
	{error, _Reason, Bin} ->
	    {error, invalid_client_id, ConnFlags, Bin}
    end.
	 
%% ==========
%% get will topic
%% ==========
get_will_topic({ok, {_Usr, _Pwd, _WRtn, _WQos, 1, _ClnSess, 0} = ConnFlags, Payload, ConnPl}) ->
    case yams_mqtt_lib:decode_str(Payload) of
	{ok, Str, RBin} ->
	    {ok, ConnFlags, RBin, ConnPl#conn_pl{ wt = Str }};
	{error, _Reason, Bin} ->
	    {error, invalid_will_topic, ConnFlags, Bin}
    end;
get_will_topic(Error) ->
    Error.

%% ==========
%% get will message
%% ==========
get_will_msg({ok, {_Usr, _Pwd, _WRtn, _WQos, 1, _ClnSess, 0} = ConnFlags, Payload, ConnPl}) ->
    case yams_mqtt_lib:decode_str(Payload) of
	{ok, Str, RBin} ->
	    {ok, ConnFlags, RBin, ConnPl#conn_pl{ wm = Str }};
	{error, _Reason, _Bin} ->
	    {error, invalid_will_msg, ConnFlags, Payload}
    end;
get_will_msg(Error) ->
    Error.

%% ==========
%% get user name
%% ==========
get_usr({ok, {1, _Pwd, _WRtn, _WQos, _will, _ClnSess, 0} = ConnFlags, Payload, ConnPl}) ->
    case yams_mqtt_lib:decode_str(Payload) of
	{ok, Str, RBin} ->
	    {ok, ConnFlags, RBin, ConnPl#conn_pl{ usr = Str }};
	{error, _Reason, Bin} ->
	    {error, invalid_user, ConnFlags, Bin}
    end;
get_usr(Error) ->
    Error.

%% ==========
%% get password
%% ==========
get_pwd({ok, {1, 1, _WRtn, _WQos, _will, _ClnSess, 0} = ConnFlags, Payload, ConnPl}) ->
    case yams_mqtt_lib:decode_str(Payload) of
	{ok, Str, RBin} ->
	    {ok, ConnFlags, RBin, ConnPl#conn_pl{ pwd = Str }};
	{error, _Reason, Bin} ->
	    {error, invalid_password, ConnFlags, Bin}
    end;
get_pwd(Error) ->
    Error.

%%===================
%% After extracting related fields from the payload,
%% the payload should remain empty.
%% Otherwise payload is too long.   
%%===================
chk_empty_payload({ok, ConnFlags, <<>>, ConnPl}) -> 
    {ok, ConnFlags, <<>>, ConnPl};
chk_empty_payload({ok, ConnFlags, Payload, _ConnPl}) -> 
    {error, payload_too_long, ConnFlags, Payload};
chk_empty_payload(Error) -> 
    Error.
