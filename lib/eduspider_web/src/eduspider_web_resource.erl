%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The resource that handles the entry of the eduspider web application
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_web_resource).

%%%_* Exports ==========================================================
-export([ init/1
        , to_html/2
        ]).

%%%_* Include ==========================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Code =============================================================
init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->
  UserIdRes = eduspider_web_lib:get_userid(ReqData),
  Params  = case UserIdRes of
              {ok, UserId}  -> [{user_id, UserId}];
              false         -> []
            end,
  {ok, Content} = home_dtl:render(Params),
  {Content, ReqData, State}.

%%%_* Internal Functions ===============================================
%% get_username(UserId) ->
%%   case user_fe:fetch(UserId) of
%%     {error, notfound} ->
%%       false;
%%     {ok, _User}        ->
%%       %%user_fe:get_email(User)
%%       UserId
%%   end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
