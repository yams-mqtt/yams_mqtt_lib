%% Record to store first byte of the MQTT message.
%% The first byte of the MQTT message contains...
%% 1. Messge Type (bit#7 to bit#4)
%% 2. Bit flag - Duplicate (bit#3)
%% 3. Bit flag - Quality of Service (bit#2 and bit#1)
%% 4. Bit flag - Retain (bit#0)
-record(type_byte, {msgtype, dup, qos, retain}).
