%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc A group of utility functions for eduspider web
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_web_lib).

%%%_* Exports ==========================================================
-export([ fetch_resource/1
        , get_userid/1
        , http_request/3
        , max_user_session_age/0
        , lookup/2
        ]).

%% -export([ fetch_fb_user_info/1
%%         ]).

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
lookup(Key, OptList) ->
  case lists:keyfind(Key, 1, OptList) of
    false ->
      throw({opt_not_found, Key});
    {Key, Value} ->
      Value
  end.

get_userid(RD) ->
  case eduspider_web_cookie:load(RD) of
    {error, Error} ->
      lager:info("fetching user id from cookie failed because: ~p", [Error]),
      false;
    {ok, UserId} ->
      {ok, UserId}
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

max_user_session_age() ->
  eduspider_web:get_app_env( max_session_age, ?default_session_age).

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
