%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Frontend module to manipulate the study material
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(article_fe).

%%%_* Exports ==========================================================
-export([ article_key_from_archive_key/1
        , article_version_from_archive_key/1
        , create/5
        , fetch/1
        , fetch_archive/2
        , get_editor/1
        , get_key/1
        , get_message/1
        , get_text/1
        , get_timestamp/1
        , get_url/1
        , get_vclock/1
        , get_version/1
        , is_archived/1
        , set_editor/2
        , set_message/2
        , set_text/2
        , store/2
        ]).

%%%_* Records ==========================================================
%% article record
-record(article, { key
                 , editor
                 , text
                 , msg
                 , version
                 , timestamp
                 , vclock
                 , is_archived
                 }).

%%%_* Types ============================================================
-type article()         :: #article{}.

%%%_* Macros ===========================================================
-define(SM_SERVICE_BASE_URL,  "http://localhost:8642/api/sm").

%%%_* Code =============================================================
%% @doc create an article object @end
-spec create( Key     :: binary()
            , Text    :: binary()
            , Message :: binary()
            , Vclock  :: binary() | list()
            , Editor  :: binary()) -> article().
create(Key, Text, Message, Vclock, Editor)
  when is_binary(Key), is_binary(Text), is_binary(Message),
       (is_binary(Vclock) orelse Vclock==undefined), is_binary(Editor) ->
  #article{ key    = Key
          , editor = Editor
          , text   = Text
          , msg    = Message
          , vclock = if Vclock == <<>> -> undefined;
                        true           -> Vclock
                     end};
create(Key, Text, Message, Vclock, Editor) when is_list(Vclock) ->
  create(Key, Text, Message, list_to_binary(Vclock), Editor).

%% @doc given the key, fetch the article from the core server @end
-spec fetch(Key :: binary()) -> {ok, article()}.
fetch(Key) ->
  case fetch_study_material(binary_to_list(Key)) of
    <<"not found">> -> {error, notfound};
    SMJason         -> {ok, from_json(SMJason)}
  end.

%% @doc given the key, fetch the article from the core server @end
-spec fetch_archive(Key :: binary(), Version :: binary()) -> {ok, article()}.
fetch_archive(Key, Version) ->
  case fetch_study_material(binary_to_list(Key), binary_to_list(Version)) of
    <<"not found">> -> {error, notfound};
    SMArcJason      -> {ok, hd(lists:map( fun set_archived/1
                                        , from_json(SMArcJason)))}
  end.

%% @doc get the editor of the article @end
-spec get_editor(Article :: article()) -> Editor :: string().
get_editor(#article{editor = Editor}) ->
  Editor.

%% @doc set the editor of the article @end
-spec set_editor(Article :: article(), Editor :: string())
                -> Article :: article().
set_editor(#article{} = Article, Editor) ->
  Article#article{editor = Editor}.


-spec get_key(Article :: article()) -> Key :: string().
get_key(#article{key = Key}) ->
  Key.

%% @doc get the message of the article @end
-spec get_message(Article :: article()) -> Msg :: string().
get_message(#article{msg = Message}) ->
  Message.

%% @doc set the message of the article @end
-spec set_message(Article :: article(), Msg :: string())
                 -> Article :: article().
set_message(#article{} = Article, Message) ->
  Article#article{msg = Message}.

%% @doc get the text of the article @end
-spec get_text(Article :: article()) -> Text :: string().
get_text(#article{text = Text}) ->
  Text.

%% @doc set the text of the article @end
-spec set_text(Article :: article(), Text :: string())
              -> Article :: article().
set_text(#article{} = Article, Text) ->
  Article#article{text = Text}.

%% @doc get the timestamp of the article @end
get_timestamp(#article{timestamp = Timestamp}) ->
  Timestamp.

%% @doc get the version of the article @end
get_vclock(#article{vclock = Vclock}) ->
  Vclock.

%% @doc get the version of the article @end
get_version(#article{version = Version}) ->
  Version.

%% TODO: should we use the authenticated user instead?
store(UserId, #article{} = Article) ->
  ArticleJson = to_json(Article),
  store_study_material(binary_to_list(UserId), ArticleJson).

%%% internal functions %%%

%% @doc set the key of the article @end
-spec set_key(Article :: article(), Key :: binary())
                 -> Article :: article().
set_key(#article{} = Article, Key) ->
  Article#article{key = Key}.

%% @doc set the vclock of the article @end
-spec set_vclock(Article :: article(), Vclock :: binary())
                 -> Article :: article().
set_vclock(#article{} = Article, Vclock) ->
  Article#article{vclock = Vclock}.

%% @doc set the timestamp of the article @end
-spec set_timestamp(Article :: article(), Timestamp :: binary())
                 -> Article :: article().
set_timestamp(#article{} = Article, Timestamp) ->
  Article#article{timestamp = Timestamp}.

%% @doc set the version of the article @end
-spec set_version(Article :: article(), Version :: binary())
                 -> Article :: article().
set_version(#article{} = Article, Version) ->
  Article#article{version = Version}.

%% @doc get the url of the article
-spec get_url(Article :: article()) -> list().
get_url(#article{key = Key0, is_archived = true}) ->
  Key = article_key_from_archive_key(Key0),
  make_url(Key);
get_url(#article{key = Key})                      ->
  make_url(Key).

make_url(Key) ->
  [ "/wiki/"
  , mochiweb_util:quote_plus(base64url:decode_to_string(Key))].

%% @doc check if an article is archived @end
-spec is_archived(Article :: article()) -> boolean().
is_archived(#article{is_archived = IsArchived}) ->
  IsArchived.

%% @doc get the article key from the archive key @end
-spec article_key_from_archive_key(ArchiveKey :: binary()) -> binary().
article_key_from_archive_key(ArchiveKey) ->
  archive_key_part(ArchiveKey, 2).

%% @doc get the article version from the archive key @end
-spec article_version_from_archive_key(ArchiveKey :: binary()) -> binary().
article_version_from_archive_key(ArchiveKey) ->
  archive_key_part(ArchiveKey, 1).

%%%_* Internal Functions ===============================================
%% @doc split the archive key into two parts and retrieve it seperately @end
archive_key_part(ArchiveKey, Part) ->
  {match, [Match]} = re:run(ArchiveKey,
                            "([^.]*)\\.(.*)",
                            [{capture, [Part], binary}]),
  Match.

%% @doc given a json struct, return an article record @end
from_json(ArticleJson) ->
  {struct, Articles} = mochijson2:decode(ArticleJson),

  F = fun({<<"article">>, {struct, ArticleFields}}, Acc) ->
          [populate_article_record(ArticleFields) | Acc]
      end,
  lists:foldl(F, [], Articles).

%% @doc convert article record to json binary
to_json(#article{} = Article) ->
  ArticleFields = { struct
                   , lists:zip( record_info(fields, article)
                              , tl(tuple_to_list(Article)))},
  mochijson2:encode({ struct
                    , [{<<"article">>, ArticleFields}] }).

%% @doc populates the article record based on the article json struct
%% @end
populate_article_record(ArticleFields) ->
  lists:foldl(fun({Key, Val}, Article) ->
                  Fun = set_article_val_fun(Key),
                  Fun(Article, Val)
              end, #article{}, ArticleFields).

%% @doc return the `set' function based on the key @end
set_article_val_fun(<<"key">>)       ->
  fun set_key/2;
set_article_val_fun(<<"editor">>)    ->
  fun set_editor/2;
set_article_val_fun(<<"text">>)      ->
  fun set_text/2;
set_article_val_fun(<<"message">>)       ->
  fun set_message/2;
set_article_val_fun(<<"version">>)   ->
  fun set_version/2;
set_article_val_fun(<<"timestamp">>) ->
  fun set_timestamp/2;
set_article_val_fun(<<"vclock">>)    ->
  fun set_vclock/2.

%% @doc set if an article is an archived version @end
-spec set_archived(article()) -> article().
set_archived(#article{} = Article) ->
  Article#article{is_archived = true}.

%% @doc fetch the latest study material using rest api @end
fetch_study_material(Key) ->
  fetch_study_material(Key, undefined).

%% @doc fetch the study material of a particular version using rest api @end
fetch_study_material(Key, Version) ->
  Url = make_fetch_study_material_url(Key, Version),
  eduspider_web_lib:fetch_resource(Url).

%% @doc store a study material using rest api @end
store_study_material(UserId, JsonBody) ->
  Url = make_store_study_material_url(UserId),
  eduspider_web_lib:http_request(Url, post, JsonBody).

%% @doc construct the url for fetching study materials @end
make_fetch_study_material_url(Key, undefined) ->
  ?SM_SERVICE_BASE_URL++"?id="++Key;
make_fetch_study_material_url(Key, Version)   ->
  ?SM_SERVICE_BASE_URL++"?id="++Key++"&version="++Version.

%% @doc construct the url for storing study materials @end
make_store_study_material_url(UserId) ->
  ?SM_SERVICE_BASE_URL++"?uid="++UserId.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
