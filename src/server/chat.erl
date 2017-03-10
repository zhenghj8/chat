%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 十二月 2016 20:45
%%%-------------------------------------------------------------------
-module(chat).
-author("Jay").

%% API
-export([start/0]).

start() ->
  io:format("chat start~n"),
  application:start(chat).
