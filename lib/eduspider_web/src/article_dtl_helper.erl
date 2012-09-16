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

%% @doc Parameterized module wrapper for Article.  Allows ErlyDTL
%%      templates to access properties of article with dotted
%%      notation.
%%
%%      Article
%%        A riak_object from either the "article" or "archive"
%%        bucket.
%%
%%      V
%%        The article revision to choose (almost always *the*
%%        revision of Article, but useful when Article has
%%        multiple revisions through riak siblings).
-module(article_dtl_helper, [ArticleVs, V]).

-export([key/0,
         key_urldecoded/0,
         path/0,
         encoded_vclock/0,
         text/0,
         html/0,
         msg/0,
         history/0]).
-export([has_multiple/0, tip_versions/0, selected_version/0]).

%% FIXME: perhaps we can remove this
-include("../../../lib/eduspider_core/include/eduspider_core.hrl").

%% @type article_props() = proplist()
%% @type history_props() = proplist()
%% @type version() = integer()

%% @spec key() -> binary()
%% @doc get the key of the article
key() ->
    ArticleV = hd(ArticleVs),
    case article_fe:is_archived(ArticleV) of
        true  -> ArchiveKey = article_fe:get_key(ArticleV),
                 article_fe:article_key_from_archive_key(ArchiveKey);
        _     -> article_fe:get_key(ArticleV)
    end.


key_urldecoded() ->
    mochiweb_util:unquote(base64url:decode_to_string(key())).

%% @spec path() -> iolist()
%% @doc get the URL-path to the article
path() ->
    article_fe:get_url(hd(ArticleVs)).

%% @spec encoded_vclock() -> binary()
%% @doc get the vclock for Article, base64-encoded
encoded_vclock() ->
    case article_fe:get_vclock(hd(ArticleVs)) of
        undefined -> <<>>;
        Vclock -> Vclock
    end.

%% @spec version_data() -> article_props()
%% @doc get the object data associated with version V
version_data() ->
    hd([ A || A <- ArticleVs, V =:= article_fe:get_version(A)]).

%% @spec text() -> binary()
%% @doc get the plain-text format of Article
text() ->
    article_fe:get_text(version_data()).

%% @spec html() -> string()
%% @doc get the html format of Article
%%      note: this is an html fragment, not a whole HTML document
html() ->
    creole:text2html(text()).

%% @spec msg() -> binary()
%% @doc get the commit message of Article
msg() ->
    article_fe:get_message(version_data()).

%% @spec history() -> [history_props()]
%% @doc get the history summary of Article
history() ->
    {ok, Summaries} = version_summary_fe:fetch(key()),
    lists:map(
      fun(Summary) ->
              [{version, version_summary_fe:get_version(Summary)},
               {msg,     version_summary_fe:get_message(Summary)},
               {editor,  version_summary_fe:get_editor(Summary)},
               {time,    version_summary_fe:get_timestamp(Summary)}]
      end,
      lists:sort(
        fun(Summary1, Summary2) ->
                version_summary_fe:get_timestamp(Summary1) >
                    version_summary_fe:get_timestamp(Summary2)
        end,
        Summaries)).

%% @spec has_multiple() -> boolean()
%% @doc true if Article contains multiple revisions (riak-siblings),
%%      false if Article contains just one revision
has_multiple() ->
    length(ArticleVs) > 1.

%% @spec tip_versions() -> [version()]
%% @doc list of revisions contained in Article
tip_versions() ->
    [ article_fe:get_version(A) || A <- ArticleVs ].

%% @spec selected_version() -> version()
%% @doc revision of Article chosen for display
selected_version() -> V.
