%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Frontend module to manipulate the study material
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

init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->
  UsernameRes   = eduspider_web_lib:get_username(ReqData),
  Username      = case UsernameRes of
                    {ok, UserName0} -> UserName0;
                    false          -> false
                  end,
  Params        = [{user_name, Username}],
  {ok, Content} = home_dtl:render(Params),
  {Content, ReqData, State}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
