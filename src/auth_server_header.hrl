%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. May 2018 13:38
%%%-------------------------------------------------------------------
-author("rafstach").

-record(user,
{
  email,
  password_hash
}).

-record(user_info,
{
  info_id,
  first_name,
  last_name,
  address,
  phone_number,
  help_question,
  answer
}).

-record(info_about,
{
  email,
  info_id
}).

-record(state,
{
  online_users = #{}
}).