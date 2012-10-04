%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Webmachine resource to http redirect
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%_* Module declaration ===============================================
-module(redirect_resource).

%%%_* Exports ==========================================================
-export([ init/1
        , resource_exists/2
        , previously_existed/2
        , moved_permanently/2]).

%%%_* Include ==========================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Code =============================================================
init(Target) ->
  {ok, Target}.

resource_exists(RD, Target) ->
  {false, RD, Target}.

previously_existed(RD, Target) ->
  {true, RD, Target}.

moved_permanently(RD, Target) ->
  {{true, Target}, RD, Target}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
