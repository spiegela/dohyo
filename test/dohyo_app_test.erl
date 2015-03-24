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

-module(dohyo_app_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("../src/dohyo.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "Start & supervise hook handlers",
      { setup,
        fun()    ->
          mock_hook_handler_setup()
          % {ok, Pid} = dohyo_app:start(normal, []),
          % Pid
        end,
        fun mock_hook_handler_cleanup/1,
        fun handler_procs_running/0
      }
    }
  ].

%%% Unit Tests

handler_procs_running() ->
  [{Handler, _Pid, Type, [Module]}] = supervisor:which_children(dohyo_sup),
  [ ?assertEqual(article_hook_handler, Handler),
    ?assertEqual(worker, Type),
    ?assertEqual(dohyo_hook_handler, Module)
  ].

%%% Fixtures

article_schema() ->
  HookFun1 = fun(_) -> fakemod:hook("fired") end,
  HookFun2 = fun() -> fakemod:hook("fired") end,
  [ #field{ name = id,
            type = integer,
            options = #{attrs => [not_null, autoincrement]}
          },
    #field{name = title, type = string},
    #field{name = content, type = text},
    #hook{type = on_create, func = HookFun1},
    #hook{type = on_update, func = HookFun1},
    #hook{type = on_delete, func = HookFun1},
    #hook{type = on_delete_all, func = HookFun2},
    #hook{type = on_schema_create, func = HookFun2}
  ].

comment_schema() ->
  [ #field{ name = id,
            type = integer,
            options = #{attrs => [not_null, autoincrement]}
          },
    #field{name = content, type = text}
  ].

%%% Mocks

mock_hook_handler_setup() ->
  meck:new(article, [non_strict]),
  meck:expect(article, schema, [], article_schema()),
  meck:new(comment, [non_strict]),
  meck:expect(comment, schema, [], comment_schema()),
  meck:new(fakemod, [non_strict]),
  meck:expect(fakemod, hook, ["fired"], ok),
  application:set_env(sumo_db, docs, [{article, mysql}, {comment, mysql}]).

mock_hook_handler_cleanup(_Pid) ->
  dohyo_app:stop([]),
  meck:unload(article),
  meck:unload(comment),
  meck:unload(fakemod),
  application:unset_env(sumo_db, docs).
