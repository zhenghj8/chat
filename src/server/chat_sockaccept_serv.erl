%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2016 14:31
%%%-------------------------------------------------------------------
-module(chat_sockaccept_serv).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/1,
  start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================
start(Socket) ->
  supervisor:start_child(chat_sockserv_sup,
    {Socket,
      {?MODULE, start_link, [Socket]},
      permanent, 200000, worker, [?MODULE]}).

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Socket]) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(accept, S = #state{socket = ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  Id = id_generator:getnewid(client),
  {ok, Pid} = chat_client_manager:start(Id, AcceptSocket),
  gen_server:cast(chat_connect_serv , {connect, Id, Pid, AcceptSocket}),
  chat_sockserv_sup:start_socket(),
  gen_tcp:controlling_process(AcceptSocket, Pid),
  {noreply, S#state{socket = AcceptSocket}};

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
