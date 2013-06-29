
-define(WEBFSMTABLE, recomet_web_fsm).

-define(FSM_WAIT_TIME, 5000).
-define(FSM_WAIT_USER_TICK, 2).
-define(WEB_WAIT_TIME, 30000).


-record(web_event, {
        type,      %% :: login | send | logout | waiting_msg|waiting_user
        message,   %% :: #web_message 
        params     %% :: list()
    }).

-record(web_state , {
        pid ,      %% :: pid()
        type,      %% :: login | send | logout | waiting_msg|waiting_user
        prev,      %% :: login | send | logout | waiting_msg|waiting_user
        message,   %% :: #message
        tick=0,    %% :: integer
        params,    %% :: list()
        start      %% :: #web_message 
    }).


