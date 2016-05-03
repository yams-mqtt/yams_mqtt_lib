%% Message Types
-type pkttype() :: 'connect'
		 | 'connack' 
		 | 'publish'
		 | 'puback'
		 | 'pubrec'
		 | 'pubrel'
		 | 'pubcomp'
		 | 'subscribe'
		 | 'suback'
		 | 'unsubscribe'
		 | 'unsuback'
		 | 'pingreq'
		 | 'pingresp'
		 | 'disconnect'.

-type no()    :: 0.
-type yes()   :: 1.
-type noyes() :: no() | yes().

-type atmost1()  :: 0.
-type atleast1() :: 1.
-type exact1()   :: 2.
-type qos()      :: atmost1() | atleast1() | exact1().

%% Record to store first byte of the MQTT message.
%% The first byte of the MQTT message contains...
%% 1. Messge Type (bit#7 to bit#4)
%% 2. Bit flag - Duplicate (bit#3)
%% 3. Bit flag - Quality of Service (bit#2 and bit#1)
%% 4. Bit flag - Retain (bit#0)
-record( type_byte, { pkttype::pkttype(),
			dup:: noyes(),
			qos:: qos(),
			retain:: noyes()
		      }).

%-record(type_byte, {pkttype, dup, qos, retain}).
