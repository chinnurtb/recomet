-ifndef(_recomet_types_included).
-define(_recomet_types_included, yeah).

%% struct message

-record(message, {appId :: integer(),
                  from :: integer(),
                  to :: integer(),
                  nick = "" :: string() | binary(),
                  type = "msg" :: string() | binary(),
                  content :: string() | binary(),
                  created = 0 :: integer(),
                  offline = false :: boolean(),
                  expire = 0 :: integer()}).

-endif.
