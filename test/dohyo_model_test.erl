%%% @author Aaron Spiegel
%%% @copyright 2015 Aaron Spiegel spiegela ++ [$@|gmail.com]
%%% 
%%% == License ==
%%% The MIT License
%%%
%%% Copyright (c) 2015 Aaron Spiegel
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to
%%% deal in the Software without restriction, including without limitation the
%%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%% IN THE SOFTWARE.

-module(dohyo_model_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "runs before_validate modifier",
      { setup,
        fun() -> mock_article_schema(article_schema()) end,
        fun unload_article_mock/1,
        fun returns_sumo_schema/0
      }
    }
  ].

returns_sumo_schema() ->
  ?assertEqual(sumo_schema(), dohyo_model:sumo_schema(article)).

sumo_schema() ->
  sumo:new_schema(article,
                  [ sumo:new_field(id, integer, [not_null, autoincrement]),
                    sumo:new_field(title, string),
                    sumo:new_field(content, text),
                    sumo:new_field(author_id, integer, [not_null]),
                    sumo:new_field(category_id, integer)
                  ]).


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

mock_article_schema(Schema) ->
  meck:new(article, [non_strict]),
  meck:expect(article, schema, [], Schema).

unload_article_mock(_) ->
  meck:unload(article).

