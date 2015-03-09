%%% @author Aaron Spiegel
%%% @copyright 2015 Aaron Spiegel spiegela ++ [$@|gmail.com]
%%% 
%%% == License ==
%%% The MIT License
%%%
%%% Copyright (c) 2015 Aaron Spiegel
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(dohyo_associations_test).

-compile(export_all).

% -include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [
    { "Errors on unknown association lookup",
      { setup,
        fun() ->
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:new(login, [non_strict]),
          meck:expect(login, schema, [], login_schema())
        end,
        fun(_) ->
          meck:unload(login),
          meck:unload(sumo_internal)
        end,
        fun association_lookup_badarg/0
      }
    },
    { "Polymorphic schema with missing type errors",
      fun missing_poly_type_badarg/0
    },
    { "Article with no comments fetches empty list",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_by, [comment, [{article_id, 3}]], [])
        end,
        fun(_) ->
          meck:unload(sumo_internal),
          meck:unload(sumo)
        end,
        fun article_has_comments/0
      }
    },
    { "Article with comments fetches list",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_by, [comment, [{article_id, 3}]],
                      comment_list())
        end,
        fun(_) ->
            meck:unload(sumo_internal),
            meck:unload(sumo)
        end,
        fun article_has_no_comments/0
      }
    },
    { "Article with no author fetches undefined",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_one, [author, [{id, 2}]], not_found)
        end,
        fun(_) ->
            meck:unload(sumo_internal),
            meck:unload(sumo)
        end,
        fun article_belongs_to_null_author/0
      }
    },
    { "Article with author fetches plist",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_one, [author, [{id, 2}]], author_2())
        end,
        fun(_) ->
            meck:unload(sumo_internal),
            meck:unload(sumo)
        end,
        fun article_belongs_to_author/0
      }
    },
    { "Article with author & alternate foreign_key fetches plist",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_one, [author, [{alternate_id, 2}]], author_2())
        end,
        fun(_) ->
            meck:unload(sumo_internal),
            meck:unload(sumo)
        end,
        fun article_foreign_key_belongs_to_author/0
      }
    },
    { "Article with author & alternate local_key fetches plist",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_one, [author, [{id, 4}]], author_2())
        end,
        fun(_) ->
            meck:unload(sumo_internal),
            meck:unload(sumo)
        end,
        fun article_local_key_belongs_to_author/0
      }
    },
    { "Article with no comments fetches empty id list",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:new(article, [non_strict]),
          meck:expect(article, schema, [], article_schema()),
          meck:expect(sumo, find_by, [comment, [{article_id, 3}]], [])
        end,
        fun(_) ->
          meck:unload(article),
          meck:unload(sumo_internal),
          meck:unload(sumo)
        end,
        fun article_has_no_comment_ids/0
      }
    },
    { "Article with comments fetches id list",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:new(article, [non_strict]),
          meck:expect(article, schema, [], article_schema()),
          meck:expect(sumo, find_by, [comment, [{article_id, 3}]],
                      comment_list())
        end,
        fun(_) ->
          meck:unload(sumo),
          meck:unload(article),
          meck:unload(sumo_internal)
        end,
        fun article_has_comment_ids/0
      }
    },
    { "Article with author fetches id",
      { setup,
        fun() -> 
          meck:expect(sumo, find_one, [author, [{id, 2}]], author_2())
        end,
        fun(_) -> meck:unload(sumo) end,
        fun article_belongs_to_author_id/0
      }
    },
    { "Tag with taggable page fetches plist",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo, find_one, [page, [{id, 1}]], tag_1())
        end,
        fun(_) -> meck:unload(sumo) end,
        fun tag_belongs_to_taggable_page/0
      }
    },
    { "Page with many tags fetches tags",
      { setup,
        fun() -> 
          meck:expect(sumo_internal, id_field_name, ['_'], id),
          meck:expect(sumo,
                      find_by,
                      [tag, [{taggable_id, 1},{taggable_type, page}]],
                      [tag_1(), tag_3()]
                     )
        end,
        fun(_) ->
          meck:unload(sumo),
          meck:unload(sumo_internal)
        end,
        fun page_has_many_tags_as_taggable/0
      }
    }
  ].

%%% Unit Tests

article_has_no_comments() ->
  Result = dohyo_associations:fetch(article, has_many_comments(), article_3()),
  ?_assertEqual([], Result).

article_has_comments() ->
  Result = dohyo_associations:fetch(article, has_many_comments(), article_3()),
  ?_assertEqual(comment_list(), Result).

article_belongs_to_null_author() ->
  Result = dohyo_associations:fetch(article, belongs_to_author(), article_3()),
  ?_assertEqual(undefined, Result).

article_belongs_to_author() ->
  Result = dohyo_associations:fetch(article, belongs_to_author(), article_3()),
  ?_assertEqual(author_2(), Result).

article_foreign_key_belongs_to_author() ->
  Result = dohyo_associations:fetch(article, foreign_key_belongs_to_author(),
                                    article_3()),
  ?_assertEqual(author_2(), Result).

article_local_key_belongs_to_author() ->
  Result = dohyo_associations:fetch(article, local_key_belongs_to_author(),
                                    article_3()),
  ?_assertEqual(author_2(), Result).

article_has_no_comment_ids() ->
  Result = dohyo_associations:fetch_ids(article, has_many_comments(), article_3()),
  ?_assertEqual([], Result).

article_has_comment_ids() ->
  Result = dohyo_associations:fetch_ids(article, has_many_comments(), article_3()),
  ?_assertEqual([3,4,5], Result).

article_belongs_to_author_id() ->
  Result = dohyo_associations:fetch_ids(article, belongs_to_author(), article_3()),
  ?_assertEqual(2, Result).

page_has_many_tags_as_taggable() ->
  Result = dohyo_associations:fetch(page, has_many_tags_as_page(), page_1()),
  ?_assertEqual([tag_1(), tag_3()], Result).

tag_belongs_to_taggable_page() ->
  Result = dohyo_associations:fetch(tag, belongs_to_taggable(), tag_1()),
  ?_assertEqual(page_1(), Result).

association_lookup_badarg() ->
  ?assertError(badarg, dohyo_associations:lookup(login, whale)).

missing_poly_type_badarg() ->
  ?assertError(badarg, dohyo_associations:fetch( tag,
                                                  belongs_to_taggable(),
                                                  invalid_tag_4()
                                                )
               ).

%%% Fixtures

article_3() ->
  [ {id, 3}, {title, "My Third Article"}, {content, "Some good stuf here..."},
    {author_id, 2}, {tag_id, 1}, {alternate_id, 4} ].

page_1() ->
  [ {id, 1}, {title, "About Me"}, {content, "Some good stuf here..."},
    {author_id, 2}, {tag_id, 1} ].

author_2() -> [ {id, 2}, {name, "Aaron Spiegel"} ].

comment_1() -> [ {id, 3}, {article_id, 3}, {commenter_id, 3} ].

comment_2() -> [ {id, 4}, {article_id, 3}, {commenter_id, 4} ].

comment_3() -> [ {id, 5}, {article_id, 3}, {commenter_id, 5} ].

comment_list() -> [comment_1(), comment_2(), comment_3()].

tag_1() -> [ {id, 1},
             {taggable_type, "page"},
             {taggable_id, 1},
             {name, "None"}
           ].

tag_2() -> [ {id, 2},
             {taggable_type, "article"},
             {taggable_id, 3},
             {name, "Programming"}
           ].

tag_3() -> [ {id, 3},
             {taggable_type, "page"},
             {taggable_id, 1},
             {name, "Programming"}
           ].

invalid_tag_4() -> [ {id, 4}, {taggable_id, 3}, {name, "Test"} ].

belongs_to_author() ->
  #association{type = belongs_to, name = author}.

foreign_key_belongs_to_author() ->
  #association{type = belongs_to, name = author,
               options = #{foreign_key => alternate_id}}.

local_key_belongs_to_author() ->
  #association{type = belongs_to, name = author,
               options = #{local_key => alternate_id}}.

has_many_comments() ->
  #association{type = has_many, name = comments,
               options = #{schema => comment}}.

has_many_tags_as_page() ->
  #association{type = has_many, name = tags,
               options = #{schema => tag, as => taggable}}.

belongs_to_taggable() ->
  #association{type = belongs_to, name = taggable,
               options = #{polymorphic => true}}.

has_many_as_special() ->
  #association{type = has_many, name = comments,
               options = #{schema => comment, foreign_key => special_id}}.

article_schema() ->
  [ #field{name = id, type = integer, attrs = [not_null, autoincrement]},
    #field{name = title, type = string},
    #field{name = content, type = text},
    #association{type = has_many, name = comments,
                 options = #{schema => comment}},
    #association{type = belongs_to, name = author,
                 options = #{schema => author, attrs => [not_null]}},
    #association{type = belongs_to, name = category,
                 options = #{schema => category}}
  ].

login_schema() ->
  [ #field{name = username, type = string},
    #field{name = password, type = string},
    #field{name = password_confirm, type = string},
    #association{type = has_many, name = roles, options = #{schema => role}},
    #association{type = belongs_to, name = account,
                 options = #{schema => account}},
    #modifier{type = before_validate, func = fun fakemod:before_validate/2},
    #modifier{type = after_validate, func = fun fakemod:after_validate/2},
    #modifier{type = before_commit, func = fun fakemod:before_commit/2},
    #modifier{type = before_delete, func = fun fakemod:before_delete/2},
    #modifier{type = before_delete_by, func = fun fakemod:before_delete_by/2},
    #modifier{type = after_read, func = fun fakemod:after_read/2}
  ].

