%% values return by connect
-type connect_ret() :: 'connack0' % connection accepted
		     | 'connack1' % unsupported protocol version
		     | 'connack2' % client ID rejected
		     | 'connack3' % server unavailable
		     | 'connack4' % bad user name or password
		     | 'connack5' % client not authorized
