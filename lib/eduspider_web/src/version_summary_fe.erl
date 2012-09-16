%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Frontend module to manipulate the study material versions
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(version_summary_fe).

%%%_* Exports ==========================================================
-export([ fetch/1
        , get_editor/1
        , get_message/1
        , get_timestamp/1
        , get_version/1
        , set_editor/2
        , set_message/2
        , set_timestamp/2
        , set_version/2
        ]).

%%%_* Include ==========================================================
-include_lib("eduspider_web/include/eduspider_web.hrl").

%%%_* Records ==========================================================
-record(version_summary, { version
                         , timestamp
                         , message
                         , editor
                         }).

%%%_* Types ============================================================
-type version_summary() :: #version_summary{}.

%%%_* Macros ===========================================================
-define(SM_VS_SERVICE_BASE_URL,  ?SERVICE_BASE_URL ++ "/api/sm_vs").

%%%_* Code =============================================================
-spec get_editor(version_summary()) -> Editor :: binary().
get_editor(#version_summary{editor = Editor}) ->
  Editor.

-spec get_message(version_summary()) -> Message :: binary().
get_message(#version_summary{message = Msg}) ->
  Msg.

-spec get_timestamp(version_summary()) -> Timestamp :: binary().
get_timestamp(#version_summary{timestamp = TS}) ->
  TS.

-spec get_version(version_summary()) -> Version :: binary().
get_version(#version_summary{version = Version}) ->
  Version.

-spec set_editor(version_summary(), binary) -> version_summary().
set_editor(#version_summary{} = VersionSummary, Editor) ->
  VersionSummary#version_summary{editor = Editor}.

-spec set_message(version_summary(), binary) -> version_summary().
set_message(#version_summary{} = VersionSummary, Msg) ->
  VersionSummary#version_summary{message = Msg}.

-spec set_timestamp(version_summary(), binary()) -> version_summary().
set_timestamp(#version_summary{} = VersionSummary, TS) ->
  VersionSummary#version_summary{timestamp = TS}.

-spec set_version(version_summary(), binary()) -> version_summary().
set_version(#version_summary{} = VersionSummary, Version) ->
  VersionSummary#version_summary{version = Version}.

%% @doc fetch the version summeries of a study material
-spec fetch(Key :: binary()) -> {ok, version_summary()} | {error, atom()}.
fetch(Key) ->
  case fetch_study_material_versions(Key) of
    <<"not found">> -> {error, notfound};
    SmVsJason       -> {ok, from_json(SmVsJason)}
  end.

%%%_* Internal Functions ===============================================
from_json(VersionSummariesJson) ->
  {struct, [{<<"version_history">>, VersionSummaries}]} =
    mochijson2:decode(VersionSummariesJson),

  F = fun({struct, VersionSummary}, Acc) ->
          [populate_version_summary_record(VersionSummary) | Acc]
      end,

  lists:foldl(F, [], VersionSummaries).

%% @doc populates the version summeries record based on the json struct
%%      of the version summaries
%% @end
populate_version_summary_record(VersionSummaryFields) ->
  lists:foldl(fun({Key, Val}, VersionSummary) ->
                  Fun = set_version_summary_val_fun(Key),
                  Fun(VersionSummary, Val)
              end, #version_summary{}, VersionSummaryFields).

%% @doc return the `set' function based on the key @end
set_version_summary_val_fun(<<"version">>)   ->
  fun set_version/2;
set_version_summary_val_fun(<<"timestamp">>) ->
  fun set_timestamp/2;
set_version_summary_val_fun(<<"message">>)   ->
  fun set_timestamp/2;
set_version_summary_val_fun(<<"editor">>)    ->
  fun set_editor/2.

%% @doc fetch the study material versions from the backend @end
fetch_study_material_versions(Key) ->
  Url = ?SM_VS_SERVICE_BASE_URL++"?id="++Key,
  eduspider_web_lib:fetch_resource(Url).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
