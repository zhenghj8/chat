%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 十二月 2016 23:30
%%%-------------------------------------------------------------------
-module(chat_connect_serv).
-author("Jay").

-behaviour(gen_server).

%% API
-export([start/0,
  start_link/0,
  get_socket_num/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock_num = 0}).
-record(player, {id, pid, socket, name, channel = [1], time, room}).



start() ->
  supervisor:start_child(chat_sup,
    {?MODULE,
      {?MODULE, start_link, []},
      permanent, 200000, worker, [?MODULE]}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  RoomNames = [room1, room2, room3, room4, room5, room6, room7, room8, room9, room10,
    room11, room12, room13, room14, room15, room16, room17, room18, room19, room20,
    room21, room22, room23, room24, room25, room26, room27, room28, room29, room30,
    room31, room32, room33, room34, room35, room36, room37, room38, room39, room40,
    room41, room42, room43, room44, room45, room46, room47, room48, room49, room50, room51],
  set_room_id(RoomNames, 1),
  {ok, #state{}}.

set_room_id([H | _L], 50) ->
  erlang:put(50, H);
set_room_id([H | L], N) ->
  erlang:put(N, H),
  set_room_id(L, N+1).


get_socket_num() ->
  gen_server:call(?MODULE, {socknum}).


handle_call({socknum}, _From, State = #state{sock_num = SockNum}) ->
  {reply, SockNum, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({connect, Id, Pid, _Socket}, State) ->
  Info = "connect successfully",
  gen_server:cast(Pid, {reply, Info}),
  Data = io_lib:format("~p:connect:~p >connect successfully!~n", [time(), Id]),
  mod_chat:log(Data),
  {noreply, State};

handle_cast({disconnect, Id, Room, Pid, Name}, State = #state{sock_num = SockNum}) ->
  case erlang:get({player, Id}) of
    #player{id = Id, pid = Pid, socket = _Socket, name = Name, channel = Channel, time = _, room = Room} ->
      gen_server:cast(Room, {disconn, Pid, Name}),
      channel_delete(Channel, Id, Pid, Name),
      erlang:erase({player, Id}),
      Data = io_lib:format("~p:connect:~p >Player ~p has disconnected!~n", [time(), Id, Name]),
      mod_chat:log(Data),
      NewSockNum = SockNum - 1,
      NewState = #state{sock_num = NewSockNum};
    _ ->
      NewState = State
  end,
  {noreply, NewState};

handle_cast({login, Pid, Name, Psw, Room, Gx, Gy, Id, Socket}, State = #state{sock_num = SockNum}) ->
  case erlang:get({user, Name}) of
    undefined ->
      erlang:put({user, Name}, Psw),
      Data = io_lib:format("~p:connect:~p >User ~p has registered! Password:~p~n", [time(), Id, Name, Psw]),
      mod_chat:log(Data),
      Data1 = io_lib:format("~p:connect:~p >Player ~p sign in successfully!~n", [time(), Id, Name]),
      mod_chat:log(Data1),
      Info = "Online!",
      NewData = {reply, Info},
      gen_server:cast(Pid, NewData),

      NewSockNum = SockNum + 1,
      NewState = #state{sock_num = NewSockNum},

      RoomName = erlang:list_to_atom(lists:concat(["room_", integer_to_list(Room)])),
      gen_server:cast(RoomName, {newplayer, Pid}),
      gen_server:cast(Pid, {room, RoomName, Name}),
      gen_server:cast(RoomName, {walk, {Gx, Gy}, Name, Socket, Pid}),
      Player = #player{id = Id, pid = Pid, socket = Socket, name = Name, time = time(), room = RoomName},
      erlang:put({player, Id}, Player),
      ChannelName = list_to_atom(lists:concat(["channel_1_", integer_to_list(Id rem 10)])),
      gen_server:cast(ChannelName, {channel, Pid, Id, Name, 1});
    Password ->
      if
        Password =:= Psw ->
          Info = "Online!",
          NewData = {reply, Info},
          gen_server:cast(Pid, NewData),

          NewSockNum = SockNum + 1,
          NewState = #state{sock_num = NewSockNum},

          RoomName = erlang:list_to_atom(lists:concat(["room_", integer_to_list(Room)])),

          Player = #player{id = Id, pid = Pid, socket = Socket, name = Name, time = time(), room = RoomName},
          erlang:put({player, Id}, Player),
          Data1 = io_lib:format("~p:connect:~p >Player ~p sign in successfully!~n", [time(), Id, Name]),
          mod_chat:log(Data1),
          gen_server:cast(RoomName, {newplayer, Pid}),
          gen_server:cast(Pid, {room, RoomName, Name}),
          gen_server:cast(RoomName, {walk, {Gx, Gy}, Name, Socket, Pid}),
          ChannelName = list_to_atom(lists:concat(["channel_1_", integer_to_list(Id rem 10)])),
          gen_server:cast(ChannelName, {channel, Pid, Id, Name, 1});
        true ->
          Info = "Connection broken!Error,you have already online!",
          NewData = {reply, Info},
          gen_server:cast(Pid, NewData),
          Info = "Connection broken!Error,you have already online!",
          Data1 = io_lib:format("~p:connect:~p >Player ~p sign in failed since to error password!~n", [time(), Id, Name]),
          mod_chat:log(Data1),

          NewState = State
      end
  end,
  {noreply, NewState};

handle_cast({new_channel_player, Id, Channel}, State) ->
  case erlang:get({player, Id}) of
    Player = #player{id = _, pid = _, socket = _, name = _, channel = Channels, time = _, room = _} ->
      case lists:member(Channel, Channels) of
        true ->
          [];
        false ->
          NewPlayer = Player#player{channel = [Channel | Channels]},
          erlang:put({player, Id}, NewPlayer)
      end;
    _ ->
      []
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
channel_delete([_H], Id, Pid, Name) ->
  ChannelName = list_to_atom(lists:concat(["channel_1_", integer_to_list(Id rem 10)])),
  gen_server:cast(ChannelName, {disconn, Pid, Name, 1});

channel_delete([H | L], _Id, Pid, Name) ->
  N1 = integer_to_list(H),
  ChannelName = erlang:list_to_atom(lists:concat(["channel_", N1])),
  gen_server:cast(ChannelName, {disconn, Pid, Name, H}),
  channel_delete(L, _Id, Pid, Name).
