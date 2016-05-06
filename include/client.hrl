-include("pre_proc.hrl").

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
-type topicFilter :: string().

%% subscription
-record (subscription, { topicFilter :: topicFilter()
		       , qos         :: qos()
		       }).

%% uid
-type uid() :: string().

%% mailboxId
-type mailBoxId() :: string().

%% session
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
%% TODO still need to know about will and retain messages.
-type msgState :: 'received' 
		| 'published' 
		| 'puback_received' 
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
		  , sessionId :: sessionId()
		  , messages  :: [#message{}]
		  }).
