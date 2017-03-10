%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十二月 2016 19:16
%%%-------------------------------------------------------------------
-module(chat_channel_1_serv).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/0,
  start_link/1]).

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
  supervisor:start_child(chat_sup,
    {channel_1,
      {?MODULE, start_link, [channel_1]},
      permanent, 200000, worker, [?MODULE]}),
  channel_1_start(0).


channel_1_start(9) ->
  supervisor:start_child(chat_sup,
    {channel_1_9,
      {?MODULE, start_link, [channel_1_9]},
      permanent, 200000, worker, [?MODULE]});
channel_1_start(N) ->
  N1 = integer_to_list(N),
  Name = erlang:list_to_atom(lists:concat(["channel_1_", N1])),
  supervisor:start_child(chat_sup,
    {Name,
      {?MODULE, start_link, [Name]},
      permanent, 200000, worker, [?MODULE]}),
  channel_1_start(N+1).


start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({channel, Pid, Id, Name, Channel}, State) ->
  case erlang:get(players) of
    undefined ->
      erlang:put(players, [{Pid, Name}]),
      LogData = io_lib:format("~p:connect:~p >Player ~p join in channel ~p!~n", [time(), Id, Name, Channel]),
      mod_chat:log(LogData),
      Data = {"newplayer", Name, Channel},
      channel_1_broadcase(Data),
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
          Data = {"newplayer", Name, Channel},
          channel_1_broadcase(Data),
          erlang:put({chat, Name}, time()),
          erlang:put({name_pid, Name}, Pid)
      end
  end,
  {noreply, State};

handle_cast({chat, Id, Pid, Name, Channel, Data}, State) ->
  ChannelName = list_to_atom(lists:concat(["channel_1_", integer_to_list(Id rem 10)])),
  gen_server:cast(ChannelName, {sub_chat, Pid, Name, Channel, Data}),
  {noreply, State};


handle_cast({sub_chat, Pid, Name, Channel, Data}, State) ->
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
              {Oh, Om, Os} = erlang:get({chat, Name}),
              {Nh, Nm, Ns} = time(),
              if
                Oh =:= Nh, Om =:= Nm, Os + 10 > Ns ->
                  Er = {"Please send a message in 10s after your last send_time!"},
                  gen_server:cast(Pid, {reply, Er}),
                  Data1 = io_lib:format("~p >Player ~p failed ro chat to channel 1! Reason:in 10s.~n", [time(), Name]),
                  mod_chat:log(Data1);
                true ->
                  Data2 = {"chat", Channel, Name, Data},
                  channel_1_broadcase(Data2),
                  erlang:put({chat,Name}, time()),
                  Data1 = io_lib:format("~p:channel_1 >Player ~p :~p.~n", [time(), Name, Data]),
                  mod_chat:log(Data1)
              end
          end
      end,
  {noreply, State};

handle_cast({broadcast, Data}, State) ->
  case erlang:get(players) of
    undefined ->
      [];
    Players ->
      PlayerPid = [X || {X, _} <- Players],
      send_msg(PlayerPid, Data)
  end,
  {noreply, State};

handle_cast({disconn, Pid, Name, _Channel}, State) ->
  erlang:erase({chat, Name}),
  erlang:erase({name_pid, Name}),
  case erlang:get(players) of
    undefined ->
      [];
    Players ->
      NewPlayers = lists:delete({Pid, Name}, Players),
      erlang:put(players, NewPlayers),
      Data = {"oldplayer", Name, 1},
      channel_1_broadcase(Data)
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
channel_1_broadcase(Data) ->
  Channels = [channel_1_0, channel_1_1, channel_1_2, channel_1_3, channel_1_4,
    channel_1_5, channel_1_6, channel_1_7, channel_1_8, channel_1_9],
  sub_broadcast(Channels, Data).

sub_broadcast([Channel], Data) ->
  gen_server:cast(Channel, {broadcast, Data});
sub_broadcast([H | L], Data) ->
  gen_server:cast(H, {broadcast, Data}),
  sub_broadcast(L, Data).

send_msg([], _Data) ->
  [];
send_msg([Player], Data) ->
  gen_server:cast(Player, {reply, Data});
send_msg([H | L], Data) ->
  gen_server:cast(H, {reply, Data}),
  send_msg(L, Data).