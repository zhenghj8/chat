%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 十二月 2016 20:47
%%%-------------------------------------------------------------------
{application, chat, [
  {description, "chat application"},
  {vsn, "1.0.0"},
  {modules, [chat, chat_app, chat_sup, chat_sockserv_sup,
    chat_client_manager, chat_channel_1_serv, chat_connect_serv,
    chat_log_serv, id_generator]},
  {registered, [chat_app]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {chat_app, []}},
  {env, []}
]}.