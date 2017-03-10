%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 十二月 2016 20:44
%%%-------------------------------------------------------------------
-module(chat_app).
-author("Jay").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
  io:format("chat app start~n"),
  case chat_sup:start_link() of
    {ok, Pid} ->
      io:format("chat supervisor has start,please wait...~n"),
      id_generator:start(),
      io:format("id_generator has start,please wait...~n"),
      chat_log_serv:start(),
      io:format("log server has start,please wait...~n"),
      chat_sockserv_sup:start(),
      io:format("chatserver supervisor has start,please wait...~n"),
      chat_connect_serv:start(),
      io:format("connect server has start,please wait...~n"),
      chat_channel_1_serv:start(),
      chat_channel_serv:start(),
      io:format("channels server has start,please wait...~n"),
      chat_room_serv:start(),
      io:format("rooms server has start,please wait...~n"),
      io:format("ok,the servers have all start successfully!~n"),
      {ok, Pid};
    Error ->
      io:format("chat app start failed:~p~n", [Error])
end.


stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
