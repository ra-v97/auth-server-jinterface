%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. May 2018 13:37
%%%-------------------------------------------------------------------
-module(auth_server).
-author("rafstach").

-behaviour(gen_server).

-include("auth_server_header.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({register,PID,First_name, Last_name,PasswordHash,Email,Address,Phone_number, Question, Answer},State) ->
  Msg = add_user(Email,PasswordHash,First_name, Last_name,Address,Phone_number, Question, Answer),
  PID ! Msg,
  {noreply,State};

handle_info({login,PID,Email,PasswordHash},State) ->
  Verification = verify_user(Email,PasswordHash),
  case Verification of
    {ok,Token} ->
      PID ! {ok,Token},
      {noreply,update_state(State,Email,Token)};
    _ ->
      PID ! {error,"Password is incorrect."},
      {noreply,State}
  end;

handle_info({emergency_question,PID,Email},State) ->
  PID ! get_help_question(Email),
  {noreply, State};

handle_info({answer_verification,PID,Email,Answer},State) ->
  Result = verify_answer(Email,Answer),
  PID ! Result,
  case Result of
    {ok,Token}->{noreply, update_state(State,Email,Token)};
    _ -> {noreply,State}
  end;

handle_info({change_password,PID,Email,PasswordHash,Token},State) ->
  case token_auth(State,Email,Token) of
    true -> PID ! change_password(Email,PasswordHash);
    _ -> PID ! {error,"You don't have permission to co this action"}
  end,
  {noreply,State};

handle_info({get_info,PID,Email,Token},State) ->
  case token_auth(State,Email,Token) of
    true -> PID ! give_info(Email);
    _ -> PID ! {error,"Wrong authentification"}
  end,
  {noreply,State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%%%% Internal functions
%%%%%===================================================================

%%%============================
%%%Adding new user
%%%============================
add_user(Email,PasswordHash,First_name, Last_name,Address,Phone_number, Question, Answer) ->
  add_user_impl(is_correct_email(Email),PasswordHash,First_name, Last_name,Address,Phone_number, Question, Answer).

add_user_impl({error,Msg},_,_,_,_,_,_,_) ->
  {error,Msg};

add_user_impl({ok,Email},PasswordHash,First_name, Last_name,Address,Phone_number, Question, Answer) ->
  User = #user{email = Email, password_hash = PasswordHash },
  InfoID = key_generator(Email),
  UserInfo = #user_info{
    info_id = InfoID,
    first_name = First_name ,
    last_name = Last_name,
    address = Address,
    phone_number = Phone_number,
    help_question = Question,
    answer = Answer},
  Relation = #info_about{email = Email , info_id = InfoID},
  InsertFun = fun() ->
      mnesia:write(User),
      mnesia:write(UserInfo),
      mnesia:write(Relation),
      ok
    end,
  Out = mnesia:transaction(InsertFun),
  case Out of
    {atomic, _}-> {ok,"Corectly added"};
    _ -> {error,"Incopleated transaction"}
  end.

%%%============================
%%%Removing user from database
%%%============================
%%remove_user(Email) ->
%%  {atomic,[ObjectToRemove1]}=mnesia:transaction(fun()->mnesia:read({user,Email})end) ,
%%  {atomic,[ObjectToRemove2]}=mnesia:transaction(fun()->mnesia:read({user_info,key_generator(Email)})end),
%%  {atomic,[ObjectToRemove3]}=mnesia:transaction(fun()->mnesia:read({info_about,Email})end),
%%  RM = fun() ->
%%        mnesia:delete_object(ObjectToRemove1),
%%        mnesia:delete_object(ObjectToRemove2),
%%        mnesia:delete_object(ObjectToRemove3)
%%       end,
%%  mnesia:transaction(RM).

%%%============================
%%%Login performance
%%%============================
verify_user(Email,PasswordHash) ->
  {atomic,UserAccount}=mnesia:transaction(fun()->mnesia:read({user,Email})end),
  case UserAccount of
    [] -> {error,"User does not exist."};
    [{user,_,PasswordInDatabase}] ->
      if
        PasswordHash == PasswordInDatabase ->
          Token = token_generator(Email),
          {ok,Token};
        true -> {error,"Password is incorrect."}
      end
  end.

%%%============================
%%%Password changing
%%%============================
get_help_question(Email)->
  InfoKey = key_generator(Email),
  Result = mnesia:transaction(fun()->mnesia:read({user_info,InfoKey})end),
  case Result of
    {atomic,[]} ->
      {error,"Incorrect email"};
    {atomic,[Data]} ->
      {ok,Data#user_info.help_question};
    _ -> {error,"Unknown help question error"}
  end.

verify_answer(Email, Answer) ->
  Result = mnesia:transaction(fun()->mnesia:read({user_info,key_generator(Email)})end),
  case Result of
    {atomic,[]} ->
      {error,"Incorrect email"};
    {atomic,[Data]} ->
      AnswerInDatabase = Data#user_info.answer,
      if
        AnswerInDatabase == Answer -> {ok,token_generator(Email)};
        true -> {error,"Wrong answer"}
      end
  end.

change_password(Email,PasswordHash)->
  ChangeFun = fun() ->
    [UserAccount]=mnesia:read({user,Email}),
    mnesia:delete_object(UserAccount),
    NewLog = #user{email = Email, password_hash = PasswordHash },
    mnesia:write(NewLog)
    end,
  Out = mnesia:transaction(ChangeFun),
  case Out of
    {aborted,_} -> {error,"Problem with changing password, try again"};
    _ -> {ok,"Password was changed correctly"}
  end.

%%%============================
%%%Returning information about user
%%%============================
give_info(Email) ->
  {atomic,[Result]} = mnesia:transaction(fun()->mnesia:read({user_info,key_generator(Email)})end),
  FirstName = Result#user_info.first_name,
  LastName = Result#user_info.last_name,
  {ok,{FirstName,LastName}}.

%%%============================
%%%Helpers functions
%%%============================
is_correct_email(Email) ->
  Correct_email = re:run(Email,"^[a-z0-9A-Z_%+-]+@[a-z0-9A-Z]+\\.[a-z]{1,4}"),
  case Correct_email of
    nomatch -> io:format("Invalid email"),{error,"Invalid email"};
    {match, _} ->
      DOES_EXIST = mnesia:transaction(fun()->mnesia:read({user,Email})end),
      case DOES_EXIST of
        {atomic,[]} ->
          {ok,Email};
        {atomic,_} ->io:format("Email already in database"),
          {error,"Email already in database"};
        _ ->io:format("Cannot check email in database"),
          {error,"Cannot check email in database"}
      end
  end.

key_generator(Email) when is_list(Email) ->
  [Key|_] = re:split(Email,"@",[{return,list}]),
  HashFun = fun(X) -> (X * 13 + (X+3)*(X+7) * 131-973*(X+91)) end,
  NewList =lists:map(HashFun,Key),
  Out = lists:foldl(fun(X, Sum) -> (X + Sum) rem 1000000 end, 0, NewList),
  abs(Out).

token_generator(String) ->
  Part1=integer_to_list(key_generator(String) rem 1000),
  {Y,M,D}=date(),
  {H,Min,S}=time(),
  Part2=integer_to_list(((Y*S) rem 117) + ((M*Min) rem 97) - ((D*H) rem 29)),
  Part1++"RSRDTK"++Part2.

update_state(State,Email, Token) ->
  #state{online_users = OU} = State,
  State#state{online_users = OU#{Email=>Token}}.

token_auth(State,Email,Token)->
  Users = State#state.online_users,
  case maps:is_key(Email,Users) of
    true ->
      RealToken=maps:get(Email,Users),
      RealToken == Token;
    _ -> false
  end.
