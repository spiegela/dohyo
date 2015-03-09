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

-module(dohyo_hooks_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "starts handlers for schemas containing hooks",
      { setup,
        fun mock_hook_handler_setup/0,
        fun unmock_hook_handler_setup/1,
        fun starts_and_registers_handler/0
      }
    },
    { "handler start can coexist with other sumo_db handlers",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun starts_and_registers_handler2/0
      }
    },
    { "runs on_create hook functions",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun runs_on_create/0
      }
    },
    { "runs on_update hook functions",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun runs_on_update/0
      }
    },
    { "runs on_delete hook functions",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun runs_on_delete/0
      }
    },
    { "runs on_delete_all hook functions",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun runs_on_delete_all/0
      }
    },
    { "runs on_schema_create hook functions",
      { setup,
        fun mock_hook_handler_setup2/0,
        fun unmock_hook_handler_setup/1,
        fun runs_on_schema_create/0
      }
    }
  ].

starts_and_registers_handler() ->
  ok = dohyo_hooks:start(),
  [ ?assertEqual({ok, [fakepid]}, application:get_env(sumo_db, events)),
    ?assert(meck:validate(dohyo_sup)),
    ?assertEqual(1, meck:num_calls(dohyo_sup, start_hook_handler, [article])),
    ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assert(meck:validate(comment)),
    ?assertEqual(1, meck:num_calls(comment, schema , []))
  ].

starts_and_registers_handler2() ->
  ok = dohyo_hooks:start(),
  [ ?assertEqual( {ok, [fakepid, some_hook_handler_pid]},
                  application:get_env(sumo_db, events)
                ),
    ?assert(meck:validate(dohyo_sup)),
    ?assertEqual(1, meck:num_calls(dohyo_sup, start_hook_handler, [article])),
    ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assert(meck:validate(comment)),
    ?assertEqual(1, meck:num_calls(comment, schema , []))
  ].

runs_on_create() ->
  ok = dohyo_hooks:on_create(article, []),
  [ ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assertEqual(1, meck:num_calls(fakemod, hook, ["fired"]))
  ].

runs_on_update() ->
  ok = dohyo_hooks:on_update(article, []),
  [ ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assertEqual(1, meck:num_calls(fakemod, hook, ["fired"]))
  ].

runs_on_delete() ->
  ok = dohyo_hooks:on_delete(article, 123),
  [ ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assertEqual(1, meck:num_calls(fakemod, hook, ["fired"]))
  ].

runs_on_delete_all() ->
  ok = dohyo_hooks:on_delete_all(article),
  [ ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assertEqual(1, meck:num_calls(fakemod, hook, ["fired"]))
  ].

runs_on_schema_create() ->
  ok = dohyo_hooks:on_schema_create(article),
  [ ?assert(meck:validate(article)),
    ?assertEqual(1, meck:num_calls(article, schema , [])),
    ?assertEqual(1, meck:num_calls(fakemod, hook, ["fired"]))
  ].

article_schema() ->
  HookFun1 = fun(_) -> fakemod:hook("fired") end,
  HookFun2 = fun() -> fakemod:hook("fired") end,
  [ #field{name = id, type = integer, attrs = [not_null, autoincrement]},
    #field{name = title, type = string},
    #field{name = content, type = text},
    #hook{type = on_create, func = HookFun1},
    #hook{type = on_update, func = HookFun1},
    #hook{type = on_delete, func = HookFun1},
    #hook{type = on_delete_all, func = HookFun2},
    #hook{type = on_schema_create, func = HookFun2}
  ].

comment_schema() ->
  [ #field{name = id, type = integer, attrs = [not_null, autoincrement]},
    #field{name = content, type = text}
  ].

mock_hook_handler_setup() ->
  meck:expect(dohyo_sup, start_hook_handler, [article], {ok, fakepid}),
  meck:new(article, [non_strict]),
  meck:expect(article, schema, [], article_schema()),
  meck:new(comment, [non_strict]),
  meck:expect(comment, schema, [], comment_schema()),
  meck:new(fakemod, [non_strict]),
  meck:expect(fakemod, hook, ["fired"], ok),
  application:set_env(sumo_db, docs, [{article, mysql}, {comment, mysql}]).

mock_hook_handler_setup2() ->
  mock_hook_handler_setup(),
  application:set_env(sumo_db, events, [some_hook_handler_pid]).

unmock_hook_handler_setup(_) ->
  meck:unload(dohyo_sup),
  meck:unload(article),
  meck:unload(comment),
  meck:unload(fakemod),
  application:unset_env(sumo_db, events),
  application:unset_env(sumo_db, docs).
