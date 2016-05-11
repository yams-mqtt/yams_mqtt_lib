-include("client.hrl").

%% values return by connect
-type connect_ret() :: 'connack0' % connection accepted
		     | 'connack1' % unsupported protocol version
		     | 'connack2' % client ID rejected
		     | 'connack3' % server unavailable
		     | 'connack4' % bad user name or password
		     | 'connack5' % client not authorized
		     | 'invalid_proto' % invalid protocol name 
		     | 'invalid_rsvd_flg' % invalid reserved flag
		     | 'no_conn_flags' % Connect flgas unavailable. 
		     | 'invalid_will_flgs' % invalid WQoS or WRtn or both.
		     | 'invalid_pwd_flg' % invalid password flag
		     | 'invalid_kat'. % invalid value for Keep alive time

%% supported protocol
-type protocol() :: string().

%% protocol level (3.1.1)
-type level() :: 4.

%% reserved flag
-type reserved() :: 0.

%% clean session
-type cln_sess() :: yesno(). 

%% will flag
-type will_flg() :: yesno().

%% will quality of service
-type will_qos() :: qos().

%% will retain
-type will_retain() :: yesno().

%% password falg
-type pwd_flg() :: yesno().

%% user flag
-type usr_flg() :: yesno().

%% connnect flags
-type connect_flgs() :: { usr_flg()
			, pwd_flg()
			, will_retain()
			, will_qos()
			, will_flg()
			, cln_sess()
			, reserved()
			}.

-type kat() :: non_neg_integer().

%% variable header
-record(var_head, { protocol :: protocol()
		  , level    :: level()
		  , connflgs :: connect_flgs()
		  , kat      :: kat()
		  }).
