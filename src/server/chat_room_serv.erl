%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 十二月 2016 23:38
%%%-------------------------------------------------------------------
-module(chat_room_serv).
-author("Jay").

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

-record(state, {room}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
  roomserv_start(1).

roomserv_start(50) ->
  Name = erlang:list_to_atom(lists:concat(["room_", "50"])),
  supervisor:start_child(chat_sup,
    {Name,
      {?MODULE, start_link, [Name]},
      permanent, 200000, worker, [?MODULE]});
roomserv_start(N) ->
  N1 = integer_to_list(N),
  Name = erlang:list_to_atom(lists:concat(["room_", N1])),
  supervisor:start_child(chat_sup,
    {Name,
      {?MODULE, start_link, [Name]},
      permanent, 200000, worker, [?MODULE]}),
  roomserv_start(N+1).


start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).


init([Name]) ->
  {ok, #state{room = Name}}.

get_player_num(RoomName) ->
  gen_server:call(RoomName, {get_player_num}).

handle_call({get_player_num}, _From, State) ->
  case erlang:get(players) of
    undefined ->
      Result = 0;
    Players ->
      Result = erlang:length(Players)
  end,
  {reply, Result, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({disconn, Pid, Name}, State) ->
  Grid = erlang:get(Name),
  erlang:erase(Grid),
  OldPlayers = erlang:get(players),
  NewPlayers = lists:delete(Pid, OldPlayers),
  erlang:put(players, NewPlayers),
  Data = {outroom, Name},
  send_msg(NewPlayers, Data),
  {noreply, State};

handle_cast({newplayer, Pid}, State) ->
  case erlang:get(players) of
    undefined ->
      erlang:put(players, [Pid]);
    Players ->
      erlang:put(players,[Pid | Players])
  end,
  {noreply, State};

handle_cast({walk, Grid, Name, Pid}, State = #state{room = RoomName}) ->
  case erlang:get(Grid) of
    undefined ->
      OldGrid = erlang:put(Name, Grid),
      erlang:erase(OldGrid),
      erlang:put(Grid, Pid),
      Grids = mod_chat_ets:find({horizon, Grid}),
      Players = [Y || Y <- [erlang:get(X) || X <- Grids], Y =/= undefined],
      Data = {"walk", Name, Grid},
%%      gen_server:cast(Pid, {broadcast, Players, Data}),
      send_msg(Players, Data),
      Data1 = io_lib:format("~p:~p >Player ~p has walked to ~p~n", [time(), RoomName, Name, Grid]),
      mod_chat:log(Data1);
    _ ->
      Data = {"griderror", Grid},
      gen_server:cast(Pid, {reply, Data})
  end,
  {noreply, State};

handle_cast({sleep, Name, _Pid}, State = #state{room = RoomName}) ->
  Grid = erlang:get(Name),
  Grids = mod_chat_ets:find({horizon, Grid}),
  Players = [Y || Y <- [erlang:get(X) || X <- Grids], Y =/= undefined],
  Data = {"sleep", Name},
%%  gen_server:cast(Pid, {broadcast, Players, Data}),
  send_msg(Players, Data),
  Data1 = io_lib:format("~p:~p>Player ~p is sleeping.~n", [time(), RoomName, Name]),
  mod_chat:log(Data1),
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