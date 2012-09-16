%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Frontend module to manipulate the user
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(user_fe).

%%%_* Exports ==========================================================
-export([ create/2
        , fetch/1
        , get_email/1
        , get_gender/1
        , get_key/1
        , get_login_time/1
        , get_photo/1
        , get_family_name/1
        , get_given_name/1
        , is_valid_session/1
        , set_login_time/2
        , store/1
        ]).

%%%_* Records ==========================================================
-record(user, { key
              , given_name
              , family_name
              , email
              , login_time
              , gender
              , utc_offset
              , photo
              }).

%%%_* Types ============================================================
-type user()      :: #user{}.
-type proplists() :: [{_, _}].

%%%_* Macros ===========================================================
-define(USER_SERVICE_BASE_URL,  "http://localhost:8642/api/user").

%%%_* Code =============================================================
%% @doc create a user record based on the proplist @end
-spec create(UserId :: binary(), ProfileFields :: proplists()) -> user().
create(UserId, ProfileFields) ->
  User = populate_user_record(ProfileFields),
  User#user{key = UserId}.

%% @doc fetch the user information from the back end @end
-spec fetch(binary()) -> user().
fetch(UserId) ->
  case fetch_user(UserId) of
    <<"not found">> -> {error, notfound};
    UserJason       -> {ok, from_json(UserJason)}
  end.

%% @doc store the user information into the back end @end
-spec store(user()) -> ok.
store(User) ->
  UserJson = to_json(User),
  store_user(UserJson).

-spec get_key(user()) -> binary().
get_key(#user{key = Key}) ->
  Key.

-spec set_key(User :: user(), Key :: binary()) -> user().
set_key(#user{} = User, Key) ->
  User#user{key = Key}.

-spec set_email(User :: user(), Email :: binary()) -> user().
set_email(#user{} = User, Email) ->
  User#user{email = Email}.

-spec get_email(User :: user()) -> Email :: binary().
get_email(#user{email = Email}) ->
  Email.

-spec set_name(User :: user(), NameStruct :: {struct, proplists()}) -> user().
set_name(#user{} = User, NameStruct) ->
  {struct, NameFields} = NameStruct,
  lists:foldl(fun({Key,Val}, User0) ->
                  Fun = set_user_name_val_fun(Key),
                  Fun(User0, Val)
              end, User, NameFields).

-spec set_photo(User :: user(), Photo :: binary()) -> user().
set_photo(#user{} = User, Photo) ->
  User#user{photo = Photo}.

-spec get_photo(User :: user()) -> Photo :: binary().
get_photo(#user{photo = Photo}) ->
  Photo.

-spec set_gender(User :: user(), Gender :: binary()) -> user().
set_gender(#user{} = User, Gender) ->
  User#user{gender = Gender}.

-spec get_gender(User :: user()) -> Gender :: binary().
get_gender(#user{gender = Gender}) ->
  Gender.

-spec set_given_name(User :: user(), GivenName :: binary()) -> user().
set_given_name(#user{} = User, GivenName) ->
  User#user{given_name = GivenName}.

-spec get_given_name(User :: user()) -> GivenName :: binary().
get_given_name(#user{given_name  = GivenName}) ->
  GivenName.

-spec set_family_name(User :: user(), FamilyName :: binary()) -> user().
set_family_name(#user{} = User, FamilyName) ->
  User#user{family_name = FamilyName}.

-spec get_family_name(User :: user()) -> FamilyName :: binary().
get_family_name(#user{family_name  = FamilyName}) ->
  FamilyName.

-spec set_login_time(User :: user(), Timestamp :: integer() | tuple())
                    -> user().
set_login_time(#user{} = User, Timestamp) when is_integer(Timestamp) ->
  User#user{login_time = Timestamp};
set_login_time(#user{} = User, Timestamp) when is_tuple(Timestamp)   ->
  Sec = timestamp_to_sec(Timestamp),
  User#user{login_time = Sec}.

-spec get_login_time(User :: user()) -> Timestamp :: integer() | tuple().
get_login_time(#user{login_time  = LoginTime}) ->
  LoginTime.

-spec set_utcoffset(User :: user(), UtcOffset :: binary()) -> user().
set_utcoffset(#user{} = User, UtcOffset) ->
  User#user{utc_offset = UtcOffset}.

-spec is_valid_session(User :: user()) -> boolean().
is_valid_session(User) ->
  LoginTime = get_login_time(User),
  Now       = timestamp_to_sec(now()),
  Max       = eduspider_web_lib:max_user_session_age(),
  (Now - LoginTime) =< Max.

%%%_* Internal Functions ===============================================
timestamp_to_sec(Timestamp) ->
  Utc = calendar:now_to_universal_time(Timestamp),
  calendar:datetime_to_gregorian_seconds(Utc).

from_json(UserJson) ->
  {struct, [User]} = mochijson2:decode(UserJson),
  {<<"user">>, {struct, UserFields}} = User,
  populate_user_record(UserFields).

populate_user_record(UserFields) ->
  lists:foldl(fun({Key, Val}, User) ->
                  Fun = set_user_val_fun(Key),
                  Fun(User, Val)
              end, #user{}, UserFields).

set_user_val_fun(<<"key">>)        ->
  fun set_key/2;
set_user_val_fun(<<"email">>)      ->
  fun set_email/2;
set_user_val_fun(<<"name">>)       ->
  fun set_name/2;
set_user_val_fun(<<"photo">>)      ->
  fun set_photo/2;
set_user_val_fun(<<"gender">>)     ->
  fun set_gender/2;
set_user_val_fun(<<"login_time">>) ->
  fun set_login_time/2;
set_user_val_fun(<<"utcOffset">>) ->
  fun set_utcoffset/2;
set_user_val_fun(_)                ->
  fun(User, _) ->
      User
  end.

set_user_name_val_fun(<<"givenName">>)  ->
  fun set_given_name/2;
set_user_name_val_fun(<<"familyName">>) ->
  fun set_family_name/2;
set_user_name_val_fun(_)                ->
  fun(User, _) ->
      User
  end.

to_json(#user{} = User) ->
  UserFields = { struct
               , lists:zip( record_info(fields, user)
                          , tl(tuple_to_list(User)))},
  mochijson2:encode({ struct
                    , [{<<"user">>, UserFields}] }).

%% @doc fetch the user information from the backend @end
fetch_user(Key) ->
  Url = ?USER_SERVICE_BASE_URL++"?id="++Key,
  eduspider_web_lib:fetch_resource(Url).

%% @doc store the user information to the backend @end
store_user(UserJson) ->
  eduspider_web_lib:http_request( ?USER_SERVICE_BASE_URL
                                , post
                                , UserJson).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
