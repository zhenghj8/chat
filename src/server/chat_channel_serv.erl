%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 十二月 2016 10:25
%%%-------------------------------------------------------------------
-module(chat_channel_serv).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/0,
  start_link/1,
  get_player_num/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
  channelserv_start(2).

channelserv_start(11) ->
  supervisor:start_child(chat_sup,
    {channel_11,
      {?MODULE, start_link, [channel_11]},
      permanent, 200000, worker, [?MODULE]});
channelserv_start(N) ->
  N1 = integer_to_list(N),
  Name = erlang:list_to_atom(lists:concat(["channel_", N1])),
  supervisor:start_child(chat_sup,
    {Name,
      {?MODULE, start_link, [Name]},
      permanent, 200000, worker, [?MODULE]}),
  channelserv_start(N+1).




start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.


get_player_num(ChannelName) ->
  gen_server:call(ChannelName, {get_player_num}).

handle_call({get_player_num}, _From, State) ->
  case erlang:get(players) of
    undefined ->
      Result = 0;
    Players ->
      Result = length(Players)
  end,
  {reply, Result, State};


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({channel, Pid, Id, Name, Channel}, State) ->
  case erlang:get(players) of
    undefined ->
      erlang:put(players, [{Pid, Name}]),
      LogData = io_lib:format("~p:connect:~p >Player ~p join in channel ~p!~n", [time(), Id, Name, Channel]),
      mod_chat:log(LogData),
      Data = {"newplayer", Name, Channel},
      gen_server:cast(Pid, {reply, Data}),
      Data1 = {new_channel_player, Id, Channel},
      gen_server:cast(chat_connect_serv, Data1),
      erlang:put({chat, Name}, time()),
      erlang:put({name_pid, Name}, Pid);
    Players ->
      case lists:member({Pid, Name}, Players) of
        true ->
          [];
        false ->
          NewPlayers = [{Pid, Name} | Players],
          erlang:put(players, NewPlayers),
          LogData = io_lib:format("~p:connect:~p >Player ~p join in channel ~p!~n", [time(), Id, Name, Channel]),
          mod_chat:log(LogData),
          PlayerPid = [X || {X, _} <- NewPlayers],
          Data = {"newplayer", Name, Channel},
          send_msg(PlayerPid, Data),
          Data1 = {new_channel_player, Id, Channel},
          gen_server:cast(chat_connect_serv, Data1),
          erlang:put({chat, Name}, time()),
          erlang:put({name_pid, Name}, Pid)
      end
  end,
  {noreply, State};


handle_cast({chat, _Id, Pid, Name, Channel, Data}, State) ->
  case erlang:get(players) of
    undefined ->
      NewData1 = {"chaterror", Channel},
      gen_server:cast(Pid, {reply, NewData1});
    Players ->
      case lists:member({Pid, Name}, Players) of
        false ->
          NewData1 = {"chaterror", Channel},
          gen_server:cast(Pid, {reply, NewData1});
        true ->
          PlayerPid = [X || {X, _} <- Players],
          Data2 = {"chat", Channel, Name, Data},
          send_msg(PlayerPid, Data2),
          erlang:put({chat, Name}, time()),
          Data1 = io_lib:format("~p:channel_~p >Player ~p:~p.~n", [time(), Channel, Name, Data]),
          mod_chat:log(Data1)
      end
  end,
  {noreply, State};

handle_cast({disconn, Pid, Name, Channel}, State) ->
  erlang:erase({chat, Name}),
  erlang:erase({name_pid, Name}),
case erlang:get(players) of
    undefined ->
      [];
    Players ->
      NewPlayers = lists:delete({Pid, Name}, Players),
      erlang:put(players, NewPlayers),
      PL = [X || {X, _} <- NewPlayers],
      Data = {"oldplayer", Name, Channel},
      send_msg(PL, Data)
  end,
  {noreply, State};


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_msg([], _Data) ->
  [];
send_msg([Player], Data) ->
  gen_server:cast(Player, {reply, Data});
send_msg([H | L], Data) ->
  gen_server:cast(H, {reply, Data}),
  send_msg(L, Data).
