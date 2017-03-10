%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十二月 2016 14:25
%%%-------------------------------------------------------------------
-module(test).
-author("zhenghuijie").

%% API
-export([sock_num/0,
  channel_players/0,
  room_players/0]).


sock_num() ->
  gen_server:call(chat_connect_server, {socknum}).

channel_players()  ->
  sub_channel(2).

sub_channel(11) ->
  Result = gen_server:call(channel_11, {get_player_num}),
  io:format("channel_11:~p~n", [Result]);
sub_channel(N) ->
  N1 = integer_to_list(N),
  ChannelName = erlang:list_to_atom(lists:concat(["channel_", N1])),
  Result = gen_server:call(ChannelName, {get_player_num}),
  io:format("~p:~p~n", [ChannelName, Result]),
  sub_channel(N+1).

room_players() ->
  sub_room(1).

sub_room(50) ->
  Result = gen_server:call(room_50, {get_player_num}),
  io:format("room_50:~p~n", [Result]);
sub_room(N) ->
  N1 = integer_to_list(N),
  RoomName = erlang:list_to_atom(lists:concat(["room_", N1])),
  Result = gen_server:call(RoomName, {get_player_num}),
  io:format("~p:~p~n", [RoomName, Result]),
  sub_room(N+1).
