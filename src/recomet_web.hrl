
-define(WEBFSMTABLE, recomet_web_fsm).
%%
-define(FSM_RESTORE_TIME, 1000).
-define(FSM_WAIT_TIME, 5000).
-define(FSM_WAIT_USER_TICK, 2).
-define(WEB_WAIT_TIME, 30000).

-include ("recomet_types.hrl").

-type web_fsm_type() :: login | message |logout |waiting_msg |waiting_user.

-record(web_event, {
        type        :: web_fsm_type ,
        message     :: #message{}   ,
        params      :: [integer()]
    }).

-record(web_state , {
        pid         :: pid()        ,
        type        :: web_fsm_type ,
        prev        :: web_fsm_type ,
        message     :: #message{}    ,
        tick=0      :: integer()    ,
        params      :: [integer(),...],
        start       :: integer()
    }).


