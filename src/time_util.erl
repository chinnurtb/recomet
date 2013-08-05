-module(time_util).

%% API
-export([get_timestamp/0]).

get_timestamp() ->
    {Mega,Sec,_Micro} = erlang:now(),
    (Mega*1000000+Sec).
    %*1000000+Micro.

