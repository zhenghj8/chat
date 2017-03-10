%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2016 14:24
%%%-------------------------------------------------------------------
-module(chat_sockserv_sup).
-author("zhenghuijie").

-behaviour(supervisor).

%% API
-export([start/0,
  start_link/0,
  start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(TCP_OPTIONS,
  [binary, {packet, 4},
    {active, true},
    {reuseaddr, true}
%%    {nodelay, false},
%%    {delay_send, true}
     ]).

start() ->
  supervisor:start_child(chat_sup,
    {?MODULE,
      {?MODULE, start_link, []},
      permanent, 200000, worker, [?MODULE]}).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [8080]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


init([Port]) ->
  mod_chat_ets:init(),
  {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
    [{chat_sockaccept_serv,
      {chat_sockaccept_serv, start_link, [ListenSocket]}, % pass the socket!
      temporary, 1000, worker, [chat_sockaccept_serv]}
    ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
