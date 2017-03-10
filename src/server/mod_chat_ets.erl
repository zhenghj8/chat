%%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十一月 2016 9:58
%%%-------------------------------------------------------------------
-module(mod_chat_ets).
-author("zhenghuijie").

%% API
-export([init/0,
  stop/0,
  find/1]).

-record(horizon, {grid1, grid2}).

-define(HORIZONX, 2).
-define(HORIZONY, 2).

%% chanel_1


init() ->
  ets:new(horizon, [public, bag, named_table, {keypos, #horizon.grid1}, {write_concurrency, true}, {read_concurrency, true} ]),
  init_horizon_line(1, ?HORIZONX, ?HORIZONY).

stop() ->
  ets:delete(horizon).


init_horizon_line(400, X, Y) ->
  init_horizon_column(400, 1, X, Y);
init_horizon_line(N, X, Y) ->
  init_horizon_column(N, 1, X, Y),
  init_horizon_line(N+1, X, Y).

init_horizon_column(Gx, 400, X, Y) ->
  if
    (Gx - X) < 1 ->
      Gx1 = 1;
    true ->
      Gx1 = Gx - X
  end,
  if
    (400 - Y) < 1 ->
      Gy1 = 1;
    true ->
      Gy1 = 400 - Y
  end,
  if
    Gx + X > 400 ->
      Gx2 = 400;
    true ->
      Gx2 = Gx + X
  end,
  Gy2 = 400,
  set_horizon({Gx1, Gy1}, {Gx2, Gy2}, {Gx1, Gy1},{Gx, 400});
init_horizon_column(Gx, Gy, X, Y) ->
  if
    (Gx - X) < 1 ->
      Gx1 = 1;
    true ->
      Gx1 = Gx - X
  end,
  if
    (Gy - Y) < 1 ->
      Gy1 = 1;
    true ->
      Gy1 = Gy - Y
  end,
  if
    Gx + X > 400 ->
      Gx2 = 400;
    true ->
      Gx2 = Gx + X
  end,
  if
    (Gy + Y) > 400 ->
      Gy2 = 400;
    true ->
      Gy2 = Gy + Y
  end,
  set_horizon({Gx1, Gy1}, {Gx2, Gy2}, {Gx1, Gy1},{Gx, Gy}),
  init_horizon_column(Gx, Gy+1, X, Y).

set_horizon({Gx1, Gy1}, {Gx2, Gy2}, {Gx, Gy},{Gx0, Gy0}) ->
  if
    {Gx, Gy} =:= {Gx2, Gy2} ->
      H = #horizon{grid1 = {Gx0, Gy0}, grid2 = {Gx, Gy}},
      ets:insert(horizon, H);
    true ->
      if
        Gx =< Gx2, Gy < Gy2 ->
          H = #horizon{grid1 = {Gx0, Gy0}, grid2 = {Gx, Gy}},
          ets:insert(horizon, H),
          set_horizon({Gx1, Gy1}, {Gx2, Gy2}, {Gx, Gy+1},{Gx0, Gy0});
        Gy =:= Gy2 ->
          H = #horizon{grid1 = {Gx0, Gy0}, grid2 = {Gx, Gy}},
          ets:insert(horizon, H),
          set_horizon({Gx1, Gy1}, {Gx2, Gy2}, {Gx+1, Gy1},{Gx0, Gy0});
        true ->
          []
      end
  end.

find({horizon, Grid}) ->
  [X#horizon.grid2 || X <- ets:lookup(horizon, Grid)].