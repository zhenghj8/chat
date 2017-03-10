%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十二月 2016 17:32
%%%-------------------------------------------------------------------
-module(robot_client).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/1,
  start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, name}).

%%%===================================================================
%%% API
%%%===================================================================
start([Beg, Num]) ->
  B = list_to_integer(Beg),
  N = list_to_integer(Num),
  sub_start(B, N).

sub_start(_Beg, 0) ->
  [];
sub_start(Beg, N) ->
  timer:sleep(100),
  case gen_tcp:connect("localhost", 8080, [binary, {packet, 4}]) of
    {ok, Socket} ->
      Name = Beg + N,
      {ok, Pid} = robot_client:start_link(Socket, Name),
      gen_tcp:controlling_process(Socket, Pid);
    _ ->
      []
  end,
  sub_start(Beg, N - 1).


start_link(Socket, Name) ->
  gen_server:start_link(?MODULE, [Socket, Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Socket, Name]) ->
  {ok, #state{socket = Socket, name = Name}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(walk, State = #state{socket = Socket, name = _Name}) ->
  erlang:send_after(2000, self(), walk),
  Grid = {rand:uniform(400), rand:uniform(400)},
  NewData = ["walk", Grid],
  gen_tcp:send(Socket, term_to_binary(NewData)),
  {noreply, State};

handle_info(sleep, State = #state{socket = Socket, name = _Name}) ->
  erlang:send_after(10000, self(), sleep),
  NewData = ["sleep"],
  gen_tcp:send(Socket, term_to_binary(NewData)),
  {noreply, State};

handle_info(wchat, State = #state{socket = Socket, name = _Name}) ->
  erlang:send_after(30000, self(), wchat),
  Str = "0123456789abcdefghijklmnopqrstuvwxwzABCDEFGHIJKLMNOPQRSTUVWXYZ",
  Len = rand:uniform(length(Str)),
  Data = randomStr(Len, Str),
  Name1 = rand:uniform(100),
  NewData = ["wchat", Name1, Data],
  gen_tcp:send(Socket, term_to_binary(NewData)),
  {noreply, State};

handle_info(chat, State = #state{socket = Socket, name = _Name}) ->
  erlang:send_after(60000, self(), chat),
  Str = "0123456789abcdefghijklmnopqrstuvwxwzABCDEFGHIJKLMNOPQRSTUVWXYZ",
  Len = rand:uniform(length(Str)),
  Data = randomStr(Len, Str),
  NewData = ["chat", rand:uniform(11), Data],
  gen_tcp:send(Socket, term_to_binary(NewData)),
  {noreply, State};

handle_info({tcp, Socket, Data}, State = #state{socket = Socket, name = Name}) ->
  Val = binary_to_term(Data),
  case Val of
    "connect successfully" ->
      Room = rand:uniform(50),
      Gx = rand:uniform(400),
      Gy = rand:uniform(400),
      Psw = rand:uniform(1000000000),
      Str = ["login", Name, Psw, Room, Gx, Gy],
      ok = gen_tcp:send(Socket, term_to_binary(Str));
    "Online!" ->
      channel(Socket),
      channel(Socket),
      channel(Socket),
      channel(Socket),
      channel(Socket),
      erlang:send_after(2000, self(), walk),
      erlang:send_after(10000, self(), sleep),
      erlang:send_after(30000, self(), wchat),
      erlang:send_after(60000, self(), chat);
    _ ->
      []
  end,
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  {noreply, State};

handle_info({tcp_error, _Socket, _}, State) ->
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
channel(Socket) ->
  Channel = rand:uniform(10) + 1,
  NewData = ["channel", Channel],
  gen_tcp:send(Socket, term_to_binary(NewData)).


randomStr(Len, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
              end,
    [], lists:seq(1, Len)).
