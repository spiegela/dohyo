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

-module(dohyo_modifiers_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "runs before_validate modifier",
      { setup,
        fun() -> mock_login_schema(before_validate()) end,
        fun unload_login_mock/1,
        fun before_validate_runs/0
      }
    },
    { "runs after_validate modifier",
      { setup,
        fun() -> mock_login_schema(after_validate()) end,
        fun unload_login_mock/1,
        fun after_validate_runs/0
      }
    },
    { "runs before_commit modifier",
      { setup,
        fun() -> mock_login_schema(before_commit()) end,
        fun unload_login_mock/1,
        fun before_commit_runs/0
      }
    },
    { "runs before_delete modifier",
      { setup,
        fun() -> mock_login_schema(before_delete()) end,
        fun unload_login_mock/1,
        fun before_delete_runs/0
      }
    },
    { "runs before_delete_by modifier",
      { setup,
        fun() -> mock_login_schema(before_delete_by()) end,
        fun unload_login_mock/1,
        fun before_delete_by_runs/0
      }
    },
    { "runs after_read modifier",
      { setup,
        fun() -> mock_login_schema(after_read()) end,
        fun unload_login_mock/1,
        fun after_read_runs/0
      }
    },
    { "runs after_read_many modifier",
      { setup,
        fun() -> mock_login_schema(after_read_many()) end,
        fun unload_login_mock/1,
        fun after_read_many_runs/0
      }
    }
  ].

%%% Unit Tests

before_validate_runs() ->
  Plist = dohyo_modifiers:before_validate(login, login(), []),
  {_, Salt} = lists:keyfind(salt, 1, Plist),
  ?assertEqual("123456", Salt).

after_validate_runs() ->
  Plist1 = dohyo_modifiers:before_validate(login, login(), []),
  Plist2 = dohyo_modifiers:after_validate(login, Plist1, []),
  Confirm = lists:keyfind(password_confirm, 1, Plist2),
  ?assertEqual(false, Confirm).

before_commit_runs() ->
  Plist1 = dohyo_modifiers:before_validate(login, login(), []),
  Plist2 = dohyo_modifiers:after_validate(login, Plist1, []),
  Plist3 = dohyo_modifiers:before_commit(login, Plist2, []),
  {_, Pass} = lists:keyfind(password, 1, Plist3),
  ?assertEqual("12345612345", Pass).

before_delete_runs() ->
  ?assertError(
    delete_contrained,
    dohyo_modifiers:before_delete(login, before_delete(), [])
  ).

before_delete_by_runs() ->
  ?assertError(
    delete_contrained,
    dohyo_modifiers:before_delete_by(login, before_delete_by(), [])
  ).

after_read_runs() ->
  Plist = dohyo_modifiers:after_read(login, login(), []),
  {_, Dyn} = lists:keyfind(dynamic, 1, Plist),
  ?assertEqual("something cool", Dyn).

after_read_many_runs() ->
  #{ logins := Plists,
     count  := Count
   } = dohyo_modifiers:after_read_many(login, [login(), login()], []),
  [ ?assertEqual([login(), login()], Plists),
    ?assertEqual(2, Count)
  ].

%%% Fixtures

login() ->
  [ {username, "spiegela"},
    {password, "12345"},
    {password_confirm, "12345"}
  ].

before_validate() ->
  [ #modifier{type = before_validate, func = fun add_fake_random_salt/2} |
    login_schema()
  ].

after_validate() ->
  [ #modifier{type = after_validate, func = fun strip_confirmation/2} |
    before_validate()
  ].

before_commit() ->
  [ #modifier{type = before_commit, func = fun fake_encrypt_password/2} |
    after_validate()
  ].

before_delete() ->
  [ #modifier{type = before_delete, func = fun fail_to_delete/2} |
    login_schema()
  ].

before_delete_by() ->
  [ #modifier{type = before_delete_by, func = fun fail_to_delete/2} |
    login_schema()
  ].

after_read() ->
  [ #modifier{type = after_read, func = fun add_dyn_field/2} |
    login_schema()
  ].

after_read_many() ->
  [ #modifier{type = after_read_many, func = fun add_collection_count/2} |
    login_schema()
  ].

login_schema() ->
  [ #field{name = username, type = string},
    #field{name = password, type = string},
    #field{name = password_confirm, type = string}
  ].

%%% Utility Functions

add_fake_random_salt(Plist, _State) ->
  %% Shh.  It's not really random
  lists:keystore(salt, 1, Plist, {salt, "123456"}).

strip_confirmation(Plist, _State) ->
  {value, _PassTup, Plist2} = lists:keytake(password_confirm, 1, Plist),
  Plist2.

fake_encrypt_password(Plist, _State) ->
  %% Shh.  It's not really encrypted..
  {_, Salt} = lists:keyfind(salt, 1, Plist),
  {_, Pass} = lists:keyfind(password, 1, Plist),
  lists:keystore(password, 1, Plist, {password, Salt ++ Pass}).

add_dyn_field(Plist, _State) ->
  lists:keystore(dynamic, 1, Plist, {dynamic, "something cool"}).

add_collection_count(Plists, _State) ->
  #{ logins => Plists,
     count  => length(Plists)
   }.

fail_to_delete(_Plist, _State) ->
  error(delete_contrained).

unload_login_mock(_) -> meck:unload(login).

mock_login_schema(Schema) ->
  meck:new(login, [non_strict]),
  meck:expect(login, schema, [], Schema).
