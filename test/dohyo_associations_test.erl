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
  meck:expect(sumo_internal, id_field_name, [article], id),

  [ { "association key translate correctly",
      association_keys()
    },
    { "Article with comments fetches list",
      article_has_comments()
    },
    { "Article with no comments fetches empty list",
      article_has_no_comments()
    },
    { "Article with not author fetches undefined",
      article_belongs_to_null_author()
    },
    { "Article with author fetches plist",
      article_belongs_to_author()
    },
    { "Article with comments fetches id list",
      article_has_comment_ids()
    },
    { "Article with no comments fetches empty id list",
      article_has_no_comment_ids()
    },
    { "Article with author fetches id",
      article_belongs_to_author_id()
    },
    { "Errors on unknown association lookup",
      { setup,
        fun() ->
          meck:new(login, [non_strict]),
          meck:expect(login, schema, [], login_schema())
        end,
        fun(_) -> meck:unload(login) end,
        association_lookup_badarg()
      }
    }
  ].

%%% Unit Tests

article_has_no_comments() ->
  meck:expect(sumo, find_by, [comment, [{article_id, 3}]], []),
  Result = dohyo_associations:fetch(article, has_many_comments(), article_3()),
  meck:unload(sumo),
  ?_assertEqual([], Result).

article_has_comments() ->
  meck:expect(sumo, find_by, [comment, [{article_id, 3}]], comment_list()),
  Result = dohyo_associations:fetch(article, has_many_comments(), article_3()),
  meck:unload(sumo),
  ?_assertEqual(comment_list(), Result).

article_belongs_to_null_author() ->
  meck:expect(sumo, find_one, [author, [{id, 2}]], not_found),
  Result = dohyo_associations:fetch(article, belongs_to_author(), article_3()),
  meck:unload(sumo),
  ?_assertEqual(undefined, Result).

article_belongs_to_author() ->
  meck:expect(sumo, find_one, [author, [{id, 2}]], author_2()),
  Result = dohyo_associations:fetch(article, belongs_to_author(), article_3()),
  meck:unload(sumo),
  ?_assertEqual(author_2(), Result).

article_has_no_comment_ids() ->
  meck:expect(sumo, find_by, [comment, [{article_id, 3}]], []),
  Result = dohyo_associations:fetch_ids(article, has_many_comments(), article_3()),
  meck:unload(sumo),
  ?_assertEqual([], Result).

article_has_comment_ids() ->
  meck:expect(sumo, find_by, [comment, [{article_id, 3}]], comment_list()),
  meck:expect(sumo_internal, id_field_name, [comment], id),
  Result = dohyo_associations:fetch_ids(article, has_many_comments(), article_3()),
  meck:unload(sumo),
  ?_assertEqual([3,4,5], Result).

article_belongs_to_author_id() ->
  meck:expect(sumo, find_one, [author, [{id, 2}]], author_2()),
  Result = dohyo_associations:fetch_ids(article, belongs_to_author(), article_3()),
  meck:unload(sumo),
  ?_assertEqual(2, Result).

association_lookup_badarg() ->
  ?_assertError(badarg, dohyo_associations:lookup(login, whales)).

association_keys() ->
 [ ?_assertEqual(
     dohyo_associations:key(local_key, article, belongs_to_author()),
     author_id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, belongs_to_author()),
     id
   ),
   ?_assertEqual(
     dohyo_associations:key(local_key, article, has_many_comments()),
     id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, has_many_comments()),
     article_id
   ),
   ?_assertEqual(
     dohyo_associations:key(local_key, article, belongs_to_user_as_author()),
     author_id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, belongs_to_user_as_author()),
     user_id
   ),
   ?_assertEqual(
     dohyo_associations:key(local_key, article, has_many_as_special()),
     id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, has_many_as_special()),
     special_id
   ),
   ?_assertEqual(
     dohyo_associations:key(local_key, article, belongs_to_as_special()),
     special_id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, belongs_to_as_special()),
     id
   ),
   ?_assertEqual(
     dohyo_associations:key(local_key, article, has_many_comments_as_another()),
     another_id
   ),
   ?_assertEqual(
     dohyo_associations:key(foreign_key, article, has_many_comments_as_another()),
     article_id
   )
  ].


%%% Fixtures

article_3() ->
  [ {id, 3}, {title, "My Third Article"}, {content, "Some good stuf here..."},
    {author_id, 2} ].

author_2() -> [ {id, 2}, {name, "Aaron Spiegel"} ].

author_3() -> [ {id, 3}, {name, "James Doe"} ].

author_4() -> [ {id, 4}, {name, "Bob Siruncal"} ].

author_5() -> [ {id, 5}, {name, "Sarah Bellum"} ].

comment_1() -> [ {id, 3}, {article_id, 3}, {commenter_id, 3} ].

comment_2() -> [ {id, 4}, {article_id, 3}, {commenter_id, 4} ].

comment_3() -> [ {id, 5}, {article_id, 3}, {commenter_id, 5} ].

comment_list() -> [comment_1(), comment_2(), comment_3()].

belongs_to_author() ->
  #association{schema = author, type = belongs_to, name = author}.

has_many_comments() ->
  #association{schema = comment, type = has_many, name = comments}.

belongs_to_commenter() ->
  #association{schema = author, type = belongs_to, name = commenter}.

belongs_to_user_as_author() ->
  #association{schema = author, type = belongs_to, name = author,
               options = [{foreign_key, user_id}]}.

has_many_as_special() ->
  #association{schema = comment, type = has_many, name = comments,
               options = [{foreign_key, special_id}]}.

belongs_to_as_special() ->
  #association{schema = author, type = belongs_to, name = author,
               options = [{local_key, special_id}]}.

has_many_comments_as_another() ->
  #association{schema = comment, type = has_many, name = comments,
               options = [{local_key, another_id}]}.
 
login_schema() ->
  [ #field{name = username, type = string},
    #field{name = password, type = string},
    #field{name = password_confirm, type = string},
    #association{type = has_many, name = roles, schema = role},
    #association{type = belongs_to, name = account, schema = account},
    #modifier{type = before_validate, func = fun fakemod:before_validate/2},
    #modifier{type = after_validate, func = fun fakemod:after_validate/2},
    #modifier{type = before_commit, func = fun fakemod:before_commit/2},
    #modifier{type = before_delete, func = fun fakemod:before_delete/2},
    #modifier{type = before_delete_by, func = fun fakemod:before_delete_by/2},
    #modifier{type = after_read, func = fun fakemod:after_read/2}
  ].

