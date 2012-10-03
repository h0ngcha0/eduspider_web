%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module to deal with the cookies
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_web_cookie).

%%%_* Exports ==========================================================
-export([ store/2
        , remove/1
        , load/1
        ]).

%%%_* Include ==========================================================
-include_lib("eduspider_web/include/eduspider_web.hrl").

%%%_* Macros ===========================================================
-define(COOKIE_HEADER,    "__Eduspider_Cookie_Header").
-define(SALT,             eduspider_web:get_app_env(salt, "")).
-define(SECRET,           eduspider_web:get_app_env(secret, "")).
-define(SESSION_AGE_DAYS, eduspider_web:get_app_env( max_session_age
                                                   , ?default_session_age)).

%%%_* Code =============================================================
%% FIXME: Just placeholder
%% @doc store a value in the eduspider cookie @end
store(Value, RD) ->
  EncodedVal = mochiweb_util:quote_plus(encode(Value)),
  do_save(RD, EncodedVal, 3600 * 24 * ?SESSION_AGE_DAYS).

%% @doc remove a value in the eduspider cookie @end
remove(RD) ->
  do_save(RD, "", -1).

%% @doc try to retrieve the the value in the eduspider cookie @end
load(RD) ->
  case wrq:get_cookie_value(?COOKIE_HEADER, RD) of
    undefined -> {error, no_cookie};
    CookieVal -> decode(mochiweb_util:unquote(CookieVal))
  end.

%%%_* Internal Functions ===============================================
do_save(RD, EncodedVal, ExpirationTime) ->
  Opts   = [ {path, "/"}
           , {max_age, ExpirationTime}
           , {http_only, true}],
  Cookie = mochiweb_cookies:cookie(?COOKIE_HEADER, EncodedVal, Opts),
  wrq:merge_resp_headers([Cookie], RD).

encode(Value) ->
  CookieVal = {Value, get_expiry()},
  Signature = crypto:sha_mac( ?SECRET
                            , term_to_binary([CookieVal, ?SALT])),
  base64:encode( term_to_binary({CookieVal, ?SALT, Signature})).

decode(CookieVal) ->
  {{Value, Expire}, Salt, Sign} = binary_to_term(base64:decode(CookieVal)),
  case crypto:sha_mac(?SECRET, term_to_binary([{Value, Expire}, Salt])) of
    Sign ->
      case Expire >= calendar:local_time() of
        true ->
          {ok, Value};
        false ->
          {error, expired}
      end;
    _ ->
      {error, invalid}
  end.

get_expiry() ->
  {Date, Time} = calendar:local_time(),
  NewDate = calendar:gregorian_days_to_date(
              calendar:date_to_gregorian_days(Date) + ?SESSION_AGE_DAYS),
  {NewDate, Time}.
%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
