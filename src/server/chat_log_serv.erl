%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十一月 2016 16:31
%%%-------------------------------------------------------------------
-module(chat_log_serv).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start_link/0,
  start/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([]).

-define(SERVER, ?MODULE).
-define(TIME, 120000).

-record(state, {}).
start() ->
  supervisor:start_child(chat_sup,
    {?MODULE,
      {?MODULE, start_link, []},
      permanent, 200000, worker, [?MODULE]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  put(1, 12),
  erlang:send_after(?TIME, self(), loop_interval_event),
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(loop_interval_event, State) ->
  erlang:send_after(?TIME, chat_log_serv, loop_interval_event),
  mod_chat_log:log(),
  {noreply, State};

handle_info({log, Name, Data}, State) ->
  mod_chat_log:save(log, Name, Data),
  {noreply, State};

handle_info({outline, Name}, State) ->
  mod_chat_log:delete(outline, Name),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

