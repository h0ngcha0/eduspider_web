%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module that stores/fetches and manipulates the study material
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(janrain_resource).

%%%_* Exports ==========================================================
-export([ allow_missing_post/2
        , allowed_methods/2
        , finish_request/2
        , init/1
        , process_post/2
        ]).

%%%_* Includes =========================================================
-include_lib("eduspider_web/include/eduspider_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Records ==========================================================
-record(ctx, { user
             , authorized = false
             }).

%%%_* Code =============================================================
init([]) ->
  {ok, #ctx{}}.

allowed_methods(RD, Ctx) ->
  {['POST'], RD, Ctx}.

allow_missing_post(RD, Ctx) ->
  {false, RD, Ctx}.

process_post(RD, Ctx) ->
  PostBody = mochiweb_util:parse_qs(wrq:req_body(RD)),
  Janrain  = eduspider_web:get_app_env(janrain, []),
  Token    = eduspider_web_lib:opt(PostBody, "token"),
  Url      = eduspider_web_lib:opt(Janrain, urlPrefix),
  ApiKey   = eduspider_web_lib:opt(Janrain, apiKey),
  Q        = [Url, "?apiKey=", ApiKey, "&token=", Token],
  Query    = lists:flatten(Q),
  case httpc:request(Query) of
    {ok, {{_, 200, _}, _Headers, Body}} ->
      {ok, User} = make_user(mochijson2:decode(Body)),
      Ctx1 = authorize(Ctx#ctx{user = User}),
      UsernameCookie = eduspider_web_lib:make_cookie_username(User),
      SecretCookie = eduspider_web_lib:make_cookie_secret(User),
      NewRD  = wrq:merge_resp_headers([UsernameCookie, SecretCookie], RD),
      { {halt, 303}
      , wrq:set_resp_header("Location", "/", NewRD)
      , Ctx1
      };
    Other ->
      lager:error("bad user response ~p", [Other]),
      { {halt, 303}
      , wrq:set_resp_header("Location", "/login", RD)
      , Ctx
      }
  end.

finish_request(RD, Ctx) ->
  {true, RD, Ctx}.

%% authorize a user
authorize(#ctx{user = User} = Ctx) ->
  ok = user_fe:store(user_fe:set_login_time(User, now())),
  Ctx#ctx{authorized = true}.

%% always make a new object from the newly fetched data
%% i.e. we never store any persistent data in wuser object
make_user({struct, JanrainResult}) ->
  {struct, Profile} = eduspider_web_lib:opt(JanrainResult, <<"profile">>),
  Identifier        = eduspider_web_lib:opt(Profile, <<"identifier">>),
  {ok, SpiderID}    = parse_identifier(Identifier),
  {ok, user_fe:create(SpiderID, Profile)}.

%% parse the identifier URL
parse_identifier(<<"http://www.facebook.com/", _/binary>> = Id) ->
  Wrq = wrq:create( 'GET'
                  , {1, 1}
                  , binary_to_list(Id)
                  , mochiweb_headers:from_list([])
                  ),
  FbId = wrq:get_qs_value("id", Wrq),
  {ok, make_spider_id(?fb, FbId)}.

%% make a spider ID out of identifier URL
make_spider_id(Provider, ID) when is_list(ID) ->
  make_spider_id(Provider, list_to_binary(ID));
make_spider_id(?fb, ID) ->
  <<"FB-", ID/binary>>.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
