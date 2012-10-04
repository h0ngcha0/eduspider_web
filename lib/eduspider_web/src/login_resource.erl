%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc webmachine resource for login
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(login_resource).

%%%_* Exports ===================================================================
-export([ init/1
        , to_html/2
        ]).

%%%_* Includes =========================================================
-include_lib("eduspider_web/include/eduspider_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Macros ===========================================================
-define(APPNAME, "eduspider_web").

%%%_* Code =============================================================
init([]) ->
  {ok, undefined}.

to_html(RD, State) ->
  Username =
    case eduspider_web_lib:get_userid(RD) of
      false ->
        "you are not logged in";
      {ok, UserId} ->
        ["current user: ", UserId]
    end,
  {ok, C} =
    login_dtl:render([ {appname, ?APPNAME}
                     , {username, Username}
                     ]),
  {C, RD, State}.

%%%_* Emacs =====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
