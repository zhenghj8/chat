%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2016 16:26
%%%-------------------------------------------------------------------
-module(mod_chat).
-author("zhenghuijie").

%% API
-export([log/1]).

-define(LOGFILE, "log").

log(Data) ->
  'chat_log_serv' ! {log, ?LOGFILE, Data}.

