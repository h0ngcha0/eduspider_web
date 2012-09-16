%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(wiki_resource).
-export([ accept_form/2
        , allow_missing_post/2
        , allowed_methods/2
        , charsets_provided/2
        , content_types_accepted/2
        , finish_request/2
        , init/1
        , is_authorized/2
        , process_post/2
        , resource_exists/2
        , to_html/2
        ]).

-include_lib("eduspider_web/include/eduspider_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, { archive %% article being viewed if not tip
             , article_vs %% article being viewed if tip
             , requested_version %% version of article requested
             , selected_version %% version of article displayed
             , user
             }).

-define(MODE_EDIT,    "edit").
-define(MODE_HISTORY, "history").
-define(MODE_DIFF,    "diff").

init([]) ->
    {ok, #ctx{}}.

allowed_methods(RD, Ctx) ->
    {['HEAD','GET','POST','PUT'], RD, Ctx}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/x-www-form-urlencoded", accept_form}], RD, Ctx}.

is_authorized(RD, Ctx) ->
    Locked = case wrq:method(RD) of
                 'PUT'  -> true;
                 'POST' -> true;
                 _      -> in_mode(RD, ?MODE_EDIT)
             end,
    Relocate = wrq:set_resp_header("Location", "/login/", RD),
    case Locked of
      true ->
        case load_user(RD, Ctx) of
          #ctx{user = U} = UCtx when is_tuple(U) ->
            {true, RD, UCtx};
          UCtx ->
            {{halt, 303}, Relocate, UCtx}
        end;
      false ->
        {true, RD, Ctx}
    end.

load_user(RD, Ctx=#ctx{user = undefined}) ->
  User =
    case get_username(RD) of
      [_|_] = Name ->
        Key = list_to_binary(Name),
        %% case eduspider_core_server:get_user(Key) of
        case user_fe:fetch(Key) of
          {ok, U} ->
            case user_fe:is_valid_session(U) of
              true ->
                U;
              false ->
                lager:info("~p session expired", [Name]),
                none
            end;
          _ ->
            lager:info("user ~p not found", [Name]),
            none
        end;
      [] ->
        lager:info("bad cookie", []),
        none
    end,
  case User of
    none ->
      Ctx#ctx{user = none};
    _ ->
      Ctx#ctx{user = User}
  end;
load_user(_, Ctx) -> Ctx.

get_username(RD) ->
  case eduspider_web_lib:has_good_cookies(RD) of
    true ->
      eduspider_web_lib:get_cookie_username(RD);
    false ->
      []
  end.

resource_exists(RD, Ctx) ->
    Key = search_path(RD),
    case article_fe:fetch(Key) of
        {ok, ArticleVs}   ->
            {true, RD, select_version(RD, Ctx#ctx{article_vs=ArticleVs})};
        {error, notfound} ->
            {false, RD, Ctx}
    end.

select_version(RD, Ctx) ->
    case wrq:get_qs_value("v", RD) of
        ReqV=[_|_] ->
            load_version(list_to_binary(ReqV), Ctx);
        _          ->
            Ctx#ctx{selected_version=latest_version(Ctx#ctx.article_vs)}
    end.

load_version(ReqV, Ctx=#ctx{article_vs=ArticleVs}) ->
    case requested_version_in_tip(ArticleVs, ReqV) of
        true ->
            Ctx#ctx{selected_version=ReqV};
        false ->
            CtxV = Ctx#ctx{requested_version=ReqV},
            case article_fe:fetch_archive(
                   article_fe:get_key(hd(ArticleVs)), ReqV) of
                {ok, Archive} ->
                    CtxV#ctx{selected_version=ReqV,
                             archive=Archive};
                {error, notfound} ->
                    CtxV#ctx{selected_version=latest_version(ArticleVs)}
            end
    end.

latest_version(ArticleVs) ->
    [{V,_}|_] = lists:reverse(
                  lists:keysort(2, [ { article_fe:get_version(A)
                                     , article_fe:get_timestamp(A)}
                                   || A <- ArticleVs])),
    V.

requested_version_in_tip(ArticleVs, ReqV) ->
    [] /= [ A || A <- ArticleVs, ReqV == article_fe:get_version(A) ].

to_html(RD, Ctx=#ctx{article_vs=ArticleVs, selected_version=V,
                     requested_version=RV, archive=Archive}) ->
    Modes = modes(RD),
    Props = [{req, wrq_dtl_helper:new(RD)},
             {article, article_dtl_helper:new(ArticleVs, V)},
             {mode, Modes},
             {requested_version, RV}
             |if Archive /= undefined ->
                      [{archive, article_dtl_helper:new([Archive], V)}];
                 true ->
                      []
              end],
    {ok, C} = case [ M || {M,true} <- Modes ] of
                  [edit|_]    -> article_editor_dtl:render(Props);
                  [history|_] -> article_history_dtl:render(Props);
                  [diff|_]    -> article_diff_dtl:render(
                                   diff_props(RD, Ctx)++Props);
                  _           -> article_dtl:render(Props)
              end,
    {C, RD, Ctx}.

diff_props(RD, #ctx{article_vs=[A|_]}) ->
    K  = article_fe:get_key(A),
    Lv = wrq:get_qs_value("l", RD),
    Rv = wrq:get_qs_value("r", RD),
    {ok, L} = article_fe:fetch_archive(K, list_to_binary(Lv)),
    {ok, R} = article_fe:fetch_archive(K, list_to_binary(Rv)),
    %% L and R are archived articles, make it the same as article
    D = diff:diff([ {article_fe:get_version(X), article_fe:get_text(X)}
                    || X <- [L,R] ]),
    [{versions, [{left, Lv}, {right, Rv}]},
     {lines, [ [{type, case Vs of
                           [_,_] -> "common";
                           [V] -> V
                       end},
                {text, T}]
               || {Vs, T} <- D ]}].

accept_form(RD, Ctx=#ctx{user = User}) ->
    Article = article_from_rd(RD, Ctx),

    %% create an article
    UserId = user_fe:get_key(User),
    article_fe:store(UserId, Article),

    {true, RD, Ctx}.

article_from_rd(RD, Ctx) ->
    Body    = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Text    = list_to_binary(proplists:get_value("text", Body)),
    Msg     = list_to_binary(proplists:get_value("msg", Body)),
    Vclock  = proplists:get_value("vclock", Body),
    UserCtx = load_user(RD, Ctx),
    Key     = search_path(RD),
    article_fe:create( Key, Text, Msg, Vclock
                     , user_fe:get_key(UserCtx#ctx.user)).

allow_missing_post(RD, Ctx) -> {true, RD, Ctx}.

process_post(RD, Ctx) ->
    Article = article_from_rd(RD, Ctx),
    ACtx    = Ctx#ctx{ article_vs=[Article]
                     , selected_version=article_fe:get_version(Article)},
    {Content, _, _} = to_html(RD, ACtx),
    {true,
     wrq:set_resp_header(
       "Content-type", "text/html",
       wrq:set_resp_body(Content, RD)),
     ACtx}.

modes(RD) ->
    M0 = [{edit,    in_mode(RD, ?MODE_EDIT)},
          {history, in_mode(RD, ?MODE_HISTORY)},
          {diff,    in_mode(RD, ?MODE_DIFF)}],
    [{view, [] == [ X || {X, true} <- M0 ]}|M0].

in_mode(RD, ModeName) ->
    wrq:get_qs_value(ModeName, RD) /= undefined.

search_path(RD) ->
    base64url:encode(mochiweb_util:unquote(wrq:disp_path(RD))).

finish_request(RD, Ctx) ->
    case wrq:response_code(RD) of
        404 ->
            {Content, NewRD, NewCtx} =
                case in_mode(RD, ?MODE_EDIT) of
                    true  -> render_404_editor(RD, Ctx);
                    false -> render_404(RD, Ctx)
                end,
            {true,
             wrq:set_resp_header(
               "Content-type", "text/html; charset=utf-8",
               wrq:set_resp_body(Content, NewRD)),
             NewCtx};
        _ ->
            {true, RD, Ctx}
    end.

render_404_editor(RD, Ctx) ->
    Article = article_fe:create(search_path(RD),
                                list_to_binary(
                                  [<<"= This page describes ">>,
                                   base64url:decode_to_string(search_path(RD)),
                                   <<" =\n">>]),
                                <<>>,
                                undefined,
                                <<>>),
    ACtx = Ctx#ctx{article_vs=[Article],
                   selected_version=article_fe:get_version(Article)},
    to_html(RD, ACtx).

render_404(RD, Ctx) ->
    Search  = base64url:decode_to_string(search_path(RD)),
    Results = study_material:search(Search),
    {ok, C} = error_404_dtl:render([{req, wrq_dtl_helper:new(RD)},
                                    {search, Search},
                                    {results, Results}]),
    {C, RD, Ctx}.
