%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc REST API handler
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(login_resource).

%%%_* Exports ===================================================================

-export(
  [ init/1
  , to_html/2
  ]).

-import(spider_lib,
  [ opt/2
  ]).

-include_lib("eduspider_web/include/eduspider_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(ctx,
  { appname = "eduspider_web" %% avoid hardcode every where
  }
).

init([]) ->
  {ok, #ctx{}}.

to_html(RD, #ctx{appname = AppName} = Ctx) ->
  Username =
    case eduspider_web_lib:get_userid(RD) of
      false ->
        "you are not logged in";
      {ok, UserId} ->
        ["current user: ", UserId]
    end,
  {ok, C} =
    login_dtl:render([ {AppName, AppName}
                     , {username, Username}
                     ]),
  {C, RD, Ctx}.

%%%_* Emacs =====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
