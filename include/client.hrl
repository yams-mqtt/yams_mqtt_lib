-include("fixed_header.hrl").

%% clientId
-type clientId() :: string().

%% username
-type userName() :: string().

%% password
-type password() :: string().

%% status
-type status() :: 'active' | 'inactive'.


%% client
-record (client, { clientId :: clientId()
		 , userName :: userName()
		 , password :: password()
  	         , status   :: status()
		 }).

%% topicFilter
%% An expression contained in a Subscription, 
%% to indicate an interest in one or more topics. 
%% A Topic Filter can include wildcard characters.
-type topicFilter() :: string().

%% subscription
%% A Subscription comprises a Topic Filter and a maximum QoS. 
%% A Subscription is associated with a single Session. 
%% A Session can contain more than one Subscription. 
%% Each Subscription within a session has a different Topic Filter.
-record (subscription, { topicFilter :: topicFilter()
		       , qos         :: qos()
		       }).

%% uid
-type uid() :: string().

%% mailboxId
-type mailBoxId() :: string().

%% session
%% A stateful interaction between a Client and a Server. 
%% Some Sessions last only as long as the Network Connection, 
%% others can span multiple consecutive Network Connections
%% between a Client and a Server.
-record (session, { sessionId     :: uid()
		  , clientId      :: clientId()
		  , status        :: status()
		  , subscriptions :: [#subscription{}]
		  , mailBoxId     :: mailBoxId()
		  }).

%% messageId
-type messageId() :: string().

%% message status
%% TODO duplicate messages are not handled here
-type msgState() :: 'publish_received' 
		  | 'publish_sent' 
		  | 'puback_received' 
		  | 'puback_sent'
		  | 'pubrec_received'
		  | 'pubrec_sent'
		  | 'pubrel_received'
		  | 'pubrel_sent'
		  | 'pubcomp_received'
		  | 'pubcomp_sent'.

%% message
-record (message, { messageId :: messageId()
		  , qos       :: qos()
		  , msgState  :: msgState()
		  }).

%% mailbox
-record (mailbox, { mailBoxId :: mailBoxId()
		  , sessionId :: uid()
		  , messages  :: [#message{}]
		  }).
