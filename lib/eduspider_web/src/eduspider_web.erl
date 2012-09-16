%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc eduspider web
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_web).

%%%_* Exports ==========================================================
-export([ start/0
        , start_link/0
        , stop/0]).

-export([ get_app_env/2
        ]).

%%%_* Code =============================================================
start()           ->
  start_common(),
  application:start(eduspider_web).

start_link()      ->
  start_common(),
  eduspider_web_sup:start_link().

stop()            ->
  Res = application:stop(eduspider_web),
  stop_common(),
  Res.

dependent_applications() ->
  [ crypto
  , compiler
  , syntax_tools
  , lager
  , pooler
  , public_key
  , ssl
  , inets
  , webmachine
  , erlydtl
  , wiki_creole
  , riakc
  , ibrowse
  , riakhttpc
  , lhttpc
  ].

start_common() ->
  lists:foreach( fun(App) ->
                     ensure_start(App)
                 end
               , dependent_applications()).

stop_common() ->
  lists:foreach( fun(App) ->
                     application:stop(App)
                 end
               , lists:reverse(dependent_applications())).

ensure_start(App) ->
  case application:start(App) of
    {error, {already_started, App}} -> ok;
    ok                              -> ok
  end.

get_app_env(Env, Default) ->
    case application:get_env(eduspider_web, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
