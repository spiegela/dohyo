-module(dohyo_associations_test).

-compile(export_all).

% -include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "association key translate correctly",
      association_keys()
    },
    { "Article with no comments returns empty list",
      article_has_no_comments()
    },
    { "Article with not author returns undefined",
      article_belongs_to_null_author()
    },
    { "Article with author returns plist",
      article_belongs_to_author()
    }
  ].

%%% Property Tests

% property_test_() ->
%   [].

%%% Unit Tests

article_has_no_comments() ->
  meck:expect(sumo, find_by, [comments, [{article_id, 3}]], []),
  Result = dohyo_associations:fetch(article_3(), article, has_many_comments()),
  meck:unload(sumo),
  ?_assertEqual(Result, []).

article_has_comments() ->
  meck:expect(sumo, find_by, [comments, [{article_id, 3}]], comment_list()),
  Result = dohyo_associations:fetch(article_3(), article, has_many_comments()),
  meck:unload(sumo),
  ?_assertEqual(Result, comment_list()).

article_belongs_to_null_author() ->
  meck:expect(sumo, find_one, [author, [{id, 2}]], not_found),
  Result = dohyo_associations:fetch(article_3(), article, belongs_to_author()),
  meck:unload(sumo),
  ?_assertEqual(Result, undefined).

article_belongs_to_author() ->
  meck:expect(sumo, find_one, [author, [{id, 2}]], author_2()),
  Result = dohyo_associations:fetch(article_3(), article, belongs_to_author()),
  meck:unload(sumo),
  ?_assertEqual(Result, author_2()).

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


%%% Properties

%%% Generators

%%% Mocks

%%% Fixtures

article_3() ->
  [ {id, 3}, {title, "My Third Article"}, {content, "Some good stuf here..."},
    {author_id, 2} ].

author_2() -> [ {id, 2}, {name, "Aaron Spiegel"} ].

comment_1() -> [ {id, 3}, {article_id, 3}, {commenter_id, 3} ].

comment_2() -> [ {id, 4}, {article_id, 3}, {commenter_id, 4} ].

comment_3() -> [ {id, 5}, {article_id, 3}, {commenter_id, 5} ].

comment_list() -> [comment_1(), comment_2(), comment_3()].

belongs_to_author() ->
  #association{type = belongs_to, name = author}.

has_many_comments() ->
  #association{schema = article, type = has_many, name = comments}.

belongs_to_user_as_author() ->
  #association{type = belongs_to, name = author,
               options = [{foreign_key, user_id}]}.

has_many_as_special() ->
  #association{type = has_many, name = comments,
               options = [{foreign_key, special_id}]}.

belongs_to_as_special() ->
  #association{type = belongs_to, name = author,
               options = [{local_key, special_id}]}.

has_many_comments_as_another() ->
  #association{type = has_many, name = comments,
               options = [{local_key, another_id}]}.
 
