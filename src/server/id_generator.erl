%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 十二月 2016 9:26
%%%-------------------------------------------------------------------
-module(id_generator).
-author("zhenghuijie").

-behaviour(gen_server).

%% API
-export([start/0,
  getnewid/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
-record(ids, {idtype, ids}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
  mnesia:start(),
  mnesia:create_schema([node()]),
  mnesia:create_table(ids, [{type, ordered_set}, {attributes, record_info(fields, ids)}, {disc_copies, []}]),
  {ok, #state{}}.


getnewid(IdType) ->
  mnesia:force_load_table(ids),
  gen_server:call(?MODULE, {getid, IdType}).

handle_call({getid, IdType}, _From, State) ->
  F = fun() ->
    Result = mnesia:read(ids, IdType, write),
    case Result of
      [S] ->
        Id = S#ids.ids,
        NewClumn = S#ids{ids = Id + 1},
        mnesia:write(ids, NewClumn, write),
        Id;
      [] ->
        NewClumn = #ids{idtype = IdType, ids = 2},
        mnesia:write(ids, NewClumn, write),
        1
    end
      end,
  case mnesia:transaction(F) of
    {atomic, Id} ->
      {atomic, Id};
    {aborted, _Reason} ->
%%      io:format("run transaction error ~1000.p ~n", [Reason]),
      Id = 0;
    _Els ->
      Id = 1000
  end,
  {reply, Id, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


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
