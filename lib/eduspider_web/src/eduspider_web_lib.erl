%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module that stores/fetches and manipulates the study material
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_web_lib).

%%%_* Exports ==========================================================
-export( [ %% fetch_fb_user_info/1
           fetch_resource/1
         , get_cookie_username/1
         , get_username/1
         , get_val/2
         , get_val/3
         , http_request/3
         , max_user_session_age/0
         , remove_cookie/1
         , save_cookie/2
         , oauth_redirect_uri/2
         , opt/2
         ]
       ).

%%%_* Includes =========================================================
-include_lib("eduspider_web/include/eduspider_web.hrl").

%%%_* Macros ===========================================================
-define(USERNAME,                 "username").
-define(SECRET,                   "secret").
-define(CONTENT_TYPE,             "application/json; charset=utf-8").
-define(HTTP_OK,                  200).
-define(SESSION_ID_HTTP_OPTIONS,  [{connect_timeout, 4000}]).
-define(SESSION_ID_HTTP_TIMEOUT,  300000).

%% get option (no default value)
opt(OptList, Opt) ->
  case lists:keyfind(Opt, 1, OptList) of
    false ->
      throw({opt_not_found, Opt});
    {Opt, Value} ->
      Value
  end.

oauth_redirect_uri(?fb, RD) ->
  "http://" ++ wrq:get_req_header("host", RD) ++ "/oauth/" ++ ?fb.

make_cookie_username(User) ->
  make_spider_cookie(?USERNAME, user_fe:get_key(User)).

make_cookie_secret(User) ->
  Username = user_fe:get_key(User),
  make_spider_cookie(?SECRET, make_secret(Username)).

make_spider_cookie(Name, Value) ->
  MaxAge = eduspider_web:get_app_env(max_session_age, ?default_session_age),
  Opts   = [{path, "/"}, {max_age, MaxAge}],
  mochiweb_cookies:cookie(Name, Value, Opts).

get_cookie_username(RD) ->
  get_spider_cookie(RD, ?USERNAME).

%% @doc Get the username from the cookie, if cookie is not valid
%%      return false
%% @end
%% FIXME: what's the type of the webmachine request data?
-spec get_username(term()) -> {ok, string()} | false.
get_username(RD) ->
  case has_good_cookies(RD) of
    true ->
      {ok, eduspider_web_lib:get_cookie_username(RD)};
    false ->
      false
  end.

get_cookie_secret(RD) ->
  get_spider_cookie(RD, ?SECRET).

get_spider_cookie(RD, Name) ->
  case wrq:get_cookie_value(Name, RD) of
    [_ | _] = Cookie ->
      Cookie;
    _ ->
      false
  end.

has_good_cookies(RD) ->
  Username = get_cookie_username(RD),
  Secret   = get_cookie_secret(RD),
  case Username /= false orelse Secret /= false of
    true ->
      Secret == make_secret(list_to_binary(Username));
    false ->
      false
  end.

%% fetch_fb_user_info(Token) ->
%%   Query = query_user_url(?fb, Token),
%%   case httpc:request(Query) of
%%     {ok, {{_, 200, _}, _Headers, Body}} ->
%%       {struct, Profile} = mochijson2:decode(Body),
%%       ID       = proplists:get_value(<<"id">>, Profile),
%%       SpiderID = <<"FB-", ID/binary>>,
%%       UserObj0 = wuser:create(SpiderID, {struct, Profile}),
%%       UserObj1 = wuser:set_tag(UserObj0, "FB"),
%%       UserObj2 = wuser:set_fb_token(UserObj1, Token),
%%       {ok, UserObj2};
%%     {ok, {_Status, _Headers, Body}} ->
%%       {error, Body};
%%     {error, Reason} ->
%%       {error, io_lib:format("~1000p", [Reason])}
%%   end.

%% query_user_url(?fb, Token) ->
%%   "https://graph.facebook.com/me?access_token=" ++ Token.

make_secret(Bin) ->
  Salt = eduspider_web:get_app_env(salt, ""),
  NewBin = list_to_binary(Salt ++ binary_to_list(Bin)),
  [ case I of
      I when I < 10  -> I + $0;
      I when I < 36  -> I - 10 + $A;
      I when I < 62  -> I - 36 + $a;
      I when I == 62 -> $_;
      I when I == 63 -> $-
    end || <<I:6>> <= erlang:md5(NewBin)
  ].

max_user_session_age() ->
    eduspider_web:get_app_env( max_session_age, ?default_session_age).


%% @doc remove the user information from the cookie @end
%% FIXME: what is the type of the webmachine request data?
-spec remove_cookie(term()) -> term().
remove_cookie(RD0) ->
  RD = wrq:remove_resp_header(?USERNAME, RD0),
  wrq:remove_resp_header(?SECRET, RD).

%% @doc save the user information into the cookie @end
%% FIXME: what is the type of the webmachine request data?
-spec save_cookie(user_fe:user(), term()) -> term().
save_cookie(User, RD) ->
  UsernameCookie = make_cookie_username(User),
  SecretCookie   = make_cookie_secret(User),
  wrq:merge_resp_headers([UsernameCookie, SecretCookie], RD).

%% @doc get the value of a key from the proplist, crash
%%      if not found
%% @end
get_val(Key, Proplist) ->
    {Key, Val} = lists:keyfind(Key, 1, Proplist),
    Val.

%% @doc get the value of a key from the proplist, return
%%      the default value if not found
%% @end
get_val(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {Key, Val} -> Val;
        false      -> Default
    end.

%% @doc wrapper for http get @end
fetch_resource(Url) ->
  Res = eduspider_web_lib:http_request(Url, get, ""),
  {ok,{{?HTTP_OK,_},_,Body}} = Res,
  Body.

%% @doc wrapper for lhttpc request with appropriate defaults @end
http_request(Url, Action, Body) ->
  lhttpc:request( Url
                , Action
                , [{"content-type", ?CONTENT_TYPE}]
                , Body
                , ?SESSION_ID_HTTP_TIMEOUT
                , ?SESSION_ID_HTTP_OPTIONS).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
