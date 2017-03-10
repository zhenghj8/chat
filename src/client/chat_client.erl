%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十一月 2016 1:07
%%%-------------------------------------------------------------------
-module(chat_client).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([chat/2,
  loop/4,
  sub_conn/0,
  wchat/2,
  channel/1,
  walk/1,
  sleep/0,
  grid/1]).

-define(SERVER, ?MODULE).


start() ->

  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Pid = spawn(chat_client, sub_conn, []),
  {ok, Pid}.

chat(Channel, Str) ->
  gen_server:cast(?MODULE, {chat, Channel, Str}).

wchat(Name, Str1) ->
  gen_server:cast(?MODULE, {wchat, Name, Str1}).

channel(Channel) ->
  gen_server:cast(?MODULE, {channel, Channel}).

walk(Grid) ->
  gen_server:cast(?MODULE, {walk, Grid}).

sleep() ->
  gen_server:cast(?MODULE, {sleep}).

grid(Grid) ->
  gen_server:cast(?MODULE,{grid, Grid}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({chat, Channel, Str}, Pid) ->
  Pid ! {self(), {chat, Channel, Str}},
  {noreply, Pid};
handle_cast({wchat, Name, Str1}, Pid) ->
  Pid ! {self(), {wchat, Name, Str1}},
  {noreply, Pid};
handle_cast({channel, Channel}, Pid) ->
  Pid ! {self(), {channel, Channel}},
  {noreply, Pid};
handle_cast({walk, Grid}, Pid) ->
  Pid ! {self(), {walk, Grid}},
  {noreply, Pid};
handle_cast({sleep}, Pid) ->
  Pid ! {self(), {sleep}},
  {noreply, Pid};
handle_cast({grid, Grid}, Pid) ->
  Pid ! {self(), {grid, Grid}},
  {noreply, Pid};

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
sub_conn() ->
  case gen_tcp:connect("localhost", 8080, [binary, {packet, 4}]) of
    {ok, Socket} ->
      Name = rand:uniform(100000),
      Psw = rand:uniform(1000000000),
      File = integer_to_list(Name),
      {ok, F} = file:open(File, [append]),
      timer:sleep(1000),
      loop(Socket, F, Name, Psw);
    Other ->
      io:format("~p~n", [Other])
  end.

loop(Socket, F, Name, Psw) ->
  receive
    {tcp, Socket, Bin} ->
      Val = binary_to_term(Bin),
      case Val of
        "connect successfully" ->
          io:format(F, "Connect successfully!~n", []),
          io:format("Connect successfully!~n", []),
          Room = rand:uniform(50),
          Gx = rand:uniform(400),
          Gy = rand:uniform(400),
          Str = ["login", Name, Psw, Room, Gx, Gy],
          ok = gen_tcp:send(Socket, term_to_binary(Str));
        "Online!" ->
          io:format("Online successfully! My name is ~p~n", [Name]),
          io:format(F, "Online successfully! My name is ~p~n", [Name]);
        {"playerlist", PlayerList, Channel} ->
          io:format(F, "@channel ~p > PlayerList: ~p~n", [Channel, PlayerList]),
          io:format("@channel ~p > PlayerList: ~p~n", [Channel, PlayerList]);
        {"newplayer", NameTemp, Channel} ->
          io:format(F, "@channel ~p > NewPlayer: ~p~n", [Channel, NameTemp]),
          io:format("@channel ~p > NewPlayer: ~p~n", [Channel, NameTemp]);
        {"oldplayer", NameTemp, Channel} ->
          io:format(F, "@channel ~p > Player ~p outline.~n", [Channel, NameTemp]),
          io:format("@channel ~p > Player ~p outline.~n", [Channel, NameTemp]);
        {"newChannel", Channel, NewChannel} ->
          io:format(F, "You have join in channel ~p successfully!~nYour newChannelList is:~p.~n", [Channel, NewChannel]),
          io:format("You have join in channel ~p successfully!~nYour newChannelList is:~p.~n", [Channel, NewChannel]);
        {"chat", Channel, NameTemp, Data} ->
          io:format(F, "@channel ~p > player ~p :~p~n", [Channel, NameTemp, Data]),
          io:format("@channel ~p > player ~p :~p~n", [Channel, NameTemp, Data]);
        {"wchat", NameTemp, Data} ->
          io:format(F, "@wchat > player ~p send you a private message:~p~n", [NameTemp, Data]),
          io:format("@wchat > player ~p send you a private message:~p~n", [NameTemp, Data]);
        {"wchaterror", NameTemp} ->
          io:format(F, "@wchatError > Wchat error! Player ~p has not online~n", [NameTemp]),
          io:format("@wchatError > Wchat error! Player ~p has not online~n", [NameTemp]);
        {"chaterror", Channel} ->
          io:format(F, "@whatError > please join the channel ~p first!~n", [Channel]),
          io:format("@whatError > please join the channel ~p first!~n", [Channel]);
        {"griderror", Grid} ->
          io:format(F, "You can't walk to ~p~n", [Grid]),
          io:format("You can't walk to ~p~n", [Grid]);
        {"gridsuccessful", Grid} ->
          io:format(F, "You have walked to ~p~n", [Grid]),
          io:format("You have walked to ~p~n", [Grid]);
        {"walk",NameTemp, Grid} ->
          io:format(F, "Player ~p has walked to ~p~n", [NameTemp, Grid]),
          io:format("Player ~p has walked to ~p~n", [NameTemp, Grid]);
        {"sleep",NameTemp} ->
          io:format(F, "Player ~p is sleeping~n", [NameTemp]),
          io:format("Player ~p is sleeping~n", [NameTemp]);
        {outroom, NameTemp} ->
          io:format(F, "Player ~p has left the room~n", [NameTemp]),
          io:format("Player ~p has left the room~n", [NameTemp]);
        Other ->
          io:format(F, "Error: ~p~n", [Other]),
          io:format("Error: ~p~n", [Other])
      end,
      loop(Socket, F, Name, Psw);
    {_, {chat, C, Data}} ->
      NewData = ["chat", C, Data],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "It is my turn to chat:~p.~n", [Data]),
      io:format("It is my turn to chat:~p.~n", [Data]),
      loop(Socket, F, Name, Psw);
    {_, {wchat, NameTemp, Data1}} ->
      NewData = ["wchat", NameTemp, Data1],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "It is my turn to wchat ~p:~p.~n", [NameTemp, Data1]),
      io:format("It is my turn to wchat ~p:~p.~n", [NameTemp, Data1]),
      loop(Socket, F, Name, Psw);
    {_, {channel, Channel}} ->
      NewData = ["channel", Channel],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "I want to join in channel ~p.~n", [Channel]),
      io:format("I want to join in channel ~p.~n", [Channel]),
      loop(Socket, F, Name, Psw);
    {_, {walk, Grid}} ->
      NewData = ["walk", Grid],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "I want to walk to ~p~n", [Grid]),
      io:format("I want to walk to ~p~n", [Grid]),
      loop(Socket, F, Name, Psw);
    {_, {sleep}} ->
      NewData = ["sleep"],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "I'm sleeping.~n", []),
      io:format("I'm sleeping.~n", []),
      loop(Socket, F, Name, Psw);
    {_, {grid, Grid}} ->
      NewData = ["grid", Grid],
      ok = gen_tcp:send(Socket, term_to_binary(NewData)),
      io:format(F, "I want to walk to ~p~n", [Grid]),
      io:format("I want to walk to ~p~n", [Grid]),
      loop(Socket, F, Name, Psw);
    _Info ->
      io:format(F, "other ~p", [_Info]),
      io:format("other ~p", [_Info]),
      loop(Socket, F, Name, Psw)
  end.
