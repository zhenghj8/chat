 %%%-------------------------------------------------------------------
%%% @author zhenghuijie
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十一月 2016 10:34
%%%-------------------------------------------------------------------
-module(mod_chat_log).
-author("zhenghuijie").

%% API
-export([save/3,
  log/0,
  delete/2]).

-define(LOGFILE, "log").

save(log, Name, Data) ->
%%  put_name(Name),
  put_data(Name, Data).


log() ->
%%  case get_log_list() of
%%    undefined ->
%%      [];
%%    NameList ->
%%      sub_log(NameList)
%%  end.
  log_in(?LOGFILE).

delete(outline, Name) ->
  case get_log_list() of
    undefined ->
      [];
    NameList ->
      NewNameList = lists:delete(Name, NameList),
      set_log_list(NewNameList),
      case get_file_name(Name) of
        undefined ->
          [];
        File ->
          file:close(File)
      end
  end.

%%put_name(Name) ->
%%  case get_log_list() of
%%    undefined ->
%%      set_log_list([Name]);
%%    NameList ->
%%      case lists:member(Name, NameList) of
%%        true ->
%%          [];
%%        false ->
%%          NewNameList = [Name | NameList],
%%          set_log_list(NewNameList)
%%      end
%%  end.

put_data(Name, Data) ->
  case erlang:get(Name) of
    undefined ->
      erlang:put(Name, [Data]);
    [] ->
      erlang:put(Name, [Data]);
    OldData ->
      NewData = [Data | OldData],
      erlang:put(Name, NewData)
  end.

%%sub_log([]) ->
%%  [];
%%sub_log([Name]) ->
%%  log_in(Name);
%%sub_log([Name | NameList]) ->
%%  log_in(Name),
%%  sub_log(NameList).


 log_in(Name) ->
   case erlang:get(Name) of
     undefined ->
       [];
     Data ->
       case get_file_name(Name) of
         undefined ->
           {ok, File} = file:open(Name, [append]),
           io:format(File, "~s", [lists:reverse(Data)]),
           erlang:put(Name, []),
           set_file_name(Name, File);
         File ->
           io:format(File, "~s", [lists:reverse(Data)]),
           erlang:put(Name, [])
       end
   end.

get_file_name(Name) ->
  erlang:get({name_file, Name}).

set_file_name(Name, File) ->
  erlang:put({name_file, Name}, File).

get_log_list() ->
  erlang:get(name_list).

set_log_list(NameList) ->
  erlang:put(name_list, NameList).
  
