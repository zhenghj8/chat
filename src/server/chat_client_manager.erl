%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2016 14:41
%%%-------------------------------------------------------------------
-module(chat_client_manager).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id, socket, room, name}).

%%%===================================================================
%%% API
%%%===================================================================
start(Id, AcceptSocket) ->
  gen_server:start_link(?MODULE, [Id, AcceptSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Id, AcceptSocket]) ->
  {ok, #state{id = Id, socket = AcceptSocket}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({reply, Data}, State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, term_to_binary(Data)),
  {noreply, State};

handle_cast({room, RoomName, Name}, State) ->
  NewState = State#state{room = RoomName, name = Name},
  ProName = list_to_atom(lists:concat(["client_", Name])),
  erlang:register(ProName, self()),
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({tcp, Socket, Data}, State = #state{id = Id, room = Room, name = Name}) ->
  sock_requs(Id, Socket, Data, Room, Name),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State = #state{id = Id, room = Room, name = Name}) ->
  gen_server:cast(chat_connect_serv, {disconnect, Id, Room, self(), Name}),
  {noreply, State};

handle_info({tcp_error, _Socket, _}, State = #state{id = Id, room = Room, name = Name}) ->
  gen_server:cast(chat_connect_serv, {disconnect, Id, Room, self(), Name}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sock_requs(Id, Socket, Data, Room0, Name0) ->
  Str = binary_to_term(Data),
  case Str of
    ["login", Name, Psw, Room, Gx, Gy] ->
      gen_server:cast(chat_connect_serv , {login, self(), Name, Psw, Room, Gx, Gy, Id, Socket});
    ["channel", Channel] ->
      if
         Channel > 1, Channel < 12 ->
           ChannelName = erlang:list_to_atom(lists:concat(["channel_", integer_to_list(Channel)])),
           gen_server:cast(ChannelName , {channel, self(), Id, Name0, Channel});
        Channel =:= 1 ->
          Data1 = "You have already join in channel 1!",
          gen_tcp:send(Socket, term_to_binary(Data1));
        true ->
          Data1 = "You could not join in the channel!",
          gen_tcp:send(Socket, term_to_binary(Data1))
      end;
    ["wchat", Name, Data1] ->
      ProName = list_to_atom(lists:concat(["client_", Name])),
      case erlang:whereis(ProName) of
        undefined ->
          NewData = {"wchaterror", Name},
          gen_tcp:send(Socket, term_to_binary(NewData)),
          Data2 = io_lib:format("~p:connect:~p >Player ~p failed to wchat to player ~p:~p.Reason:player ~p not online.~n", [time(), Id, Name0, Name, Data1, Name]),
          mod_chat:log(Data2);
        Pid ->
          Data2 = {"wchat", Name0, Data1},
          gen_server:cast(Pid, {reply, Data2}),
          Data3 = io_lib:format("~p:connect:~p >Player ~p wchat to player ~p:~p.~n", [time(), Id, Name0, Name, Data1]),
          mod_chat:log(Data3)
      end;
    ["chat", Channel, Data1] ->
      ChannelName = erlang:list_to_atom(lists:concat(["channel_", integer_to_list(Channel)])),
      gen_server:cast(ChannelName , {chat, Id, self(), Name0, Channel, Data1});
    ["walk", Grid] ->
      gen_server:cast(Room0, {walk, Grid, Name0, self()});
    ["sleep"] ->
    gen_server:cast(Room0, {sleep, Name0, self()})
  end.
