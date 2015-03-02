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

-module(dohyo_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/dohyo.hrl").

-include("test_helpers.hrl").

%%% Unit Test Descriptions

unit_test_() ->
  [ { "wraps sumo_db persist",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_persist/0
       }
    },
    { "wraps sumo_db delete",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_delete/0
       }
    },
    { "wraps sumo_db delete_by",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_delete_by/0
       }
    },
    { "wraps sumo_db delete_all",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_delete_all/0
       }
    },
    { "wraps sumo_db find",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find/0
       }
    },
    { "wraps sumo_db find_one",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_one/0
       }
    },
    { "wraps sumo_db find_all/2",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_all_2/0
       }
    },
    { "wraps sumo_db find_all/4",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_all_4/0
       }
    },
    { "wraps sumo_db find_by/2",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_by_2/0
       }
    },
    { "wraps sumo_db find_by/4",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_by_4/0
       }
    },
    { "wraps sumo_db find_by/5",
       { setup,
         fun setup_mocks/0,
         fun unload_mocks/1,
         fun wrap_sumo_find_by_5/0
       }
    }
  ].

%%% Property Tests

property_test_() ->
  [ ?_assertEqual(?_proper_passes(field_returns_record()), true),
    ?_assertEqual(?_proper_passes(field_with_args_returns_record()), true),
    ?_assertEqual(?_proper_passes(has_many_returns_record()), true),
    ?_assertEqual(?_proper_passes(has_many_with_opts_returns_record()), true),
    ?_assertEqual(?_proper_passes(belongs_to_returns_record()), true),
    ?_assertEqual(?_proper_passes(belongs_to_with_opts_returns_record()), true),
    ?_assertEqual(?_proper_passes(validate_returns_record()), true),
    ?_assertEqual(?_proper_passes(validate_with_args_returns_record()), true),
    ?_assertEqual(?_proper_passes(before_validate_returns_record()), true),
    ?_assertEqual(?_proper_passes(after_validate_returns_record()), true),
    ?_assertEqual(?_proper_passes(before_commit_returns_record()), true),
    ?_assertEqual(?_proper_passes(before_delete_returns_record()), true),
    ?_assertEqual(?_proper_passes(before_delete_by_returns_record()), true),
    ?_assertEqual(?_proper_passes(after_read_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_create_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_update_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_delete_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_delete_all_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_schema_create_returns_record()), true)
  ].

%%% Unit Tests

wrap_sumo_persist() ->
  State = [],
  Plist = dohyo:persist(login, login(), State),
  [ ?_assertEqual(login(), Plist),
    ?_assert(meck:validate(login)),
    ?_assertEqual(4, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(1, meck:num_calls(fakemod, before_validate, [Plist, State])),
    ?_assertEqual(1, meck:num_calls(fakemod, after_validate, [Plist, State])),
    ?_assertEqual(1, meck:num_calls(fakemod, before_commit, [Plist, State])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, persist, [login, Plist])),
    ?_assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, State]))
  ].

wrap_sumo_delete() ->
  State = [],
  Id = 5,
  true = dohyo:delete(login, Id, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(2, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(1, meck:num_calls(fakemod, before_delete, [Id, State])),
    ?_assertEqual(1, meck:num_calls(sumo, delete, [login, Id]))
  ].

wrap_sumo_delete_by() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  11 = dohyo:delete_by(login, Conditions, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(2, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(1, meck:num_calls( fakemod,
                                     before_delete_by,
                                     [Conditions, State]
                                   )),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, delete_by, [login, Conditions]))
  ].

wrap_sumo_delete_all() ->
  State = [],
  22 = dohyo:delete_all(login, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(2, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, delete_all, [login]))
  ].

wrap_sumo_find() ->
  State = [],
  Plist = login(),
  Plist = dohyo:find(login, 5, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find, [login, 5])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, []]))
  ].

wrap_sumo_find_one() ->
  State = [],
  Plist = login(),
  Conditions = [{username, "spiegela"}],
  Plist = dohyo:find_one(login, Conditions, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_one, [login, Conditions])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, []]))
  ].

wrap_sumo_find_all_2() ->
  State = [],
  Plists = [login(),login(),login()],
  Plists = dohyo:find_all(login, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_all, [login])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(3, meck:num_calls(fakemod, after_read, [Plists, []]))
  ].

wrap_sumo_find_all_4() ->
  State = [],
  Plists = [login(),login(),login()],
  Plists = dohyo:find_all(login, username, 10, 10, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_all, [login, username, 10, 10])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(3, meck:num_calls(fakemod, after_read, [Plists, []]))
  ].

wrap_sumo_find_by_2() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists = [login(),login(),login()],
  Plists = dohyo:find_by(login, Conditions, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_by, Conditions)),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(3, meck:num_calls(fakemod, after_read, [Plists, []]))
  ].

wrap_sumo_find_by_4() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists = [login(),login(),login()],
  Plists = dohyo:find_by(login, Conditions, 10, 10, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_by, [login, Conditions, 10, 10])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(3, meck:num_calls(fakemod, after_read, [Plists, []]))
  ].

wrap_sumo_find_by_5() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists = [login(),login(),login()],
  Plists = dohyo:find_by(login, Conditions, username, 10, 10, State),
  [ ?_assert(meck:validate(login)),
    ?_assertEqual(1, meck:num_calls(login, schema, [])),
    ?_assert(meck:validate(sumo)),
    ?_assertEqual(1, meck:num_calls(sumo, find_by, [login, Conditions, username, 10, 10])),
    ?_assert(meck:validate(fakemod)),
    ?_assertEqual(3, meck:num_calls(fakemod, after_read, [Plists, []]))
  ].

setup_mocks() ->
  Plist = login(),
  Id = 5,
  Conditions = [{username, "spiegela"}],
  meck:new(login, [non_strict]),
  meck:expect(login, schema, [], login_schema()),
  meck:new(fakemod, [non_strict]),
  meck:expect(fakemod, before_validate, [Plist, []], Plist),
  meck:expect(fakemod, after_validate, [Plist, []], Plist),
  meck:expect(fakemod, before_commit, [Plist, []], Plist),
  meck:expect(fakemod, before_delete, [Id, []], Id),
  meck:expect(fakemod, before_delete_by, [Conditions, []], Conditions),
  meck:expect(fakemod, after_read, [Plist, []], Plist),
  meck:expect(sumo, persist, [login, Plist], Plist),
  meck:expect(sumo, delete, [login, Id], true),
  meck:expect(sumo, delete_by, [login, Conditions], 11),
  meck:expect(sumo, delete_all, [login], 22),
  meck:expect(sumo, find, [login, 5], Plist),
  meck:expect(sumo, find_one, [login, Conditions], Plist),
  meck:expect(sumo, find_all, [login], [Plist,Plist,Plist]),
  meck:expect(sumo, find_all, [login, username, 10, 10], [Plist,Plist,Plist]),
  meck:expect(sumo, find_by, [login, Conditions], [Plist,Plist,Plist]),
  meck:expect(sumo, find_by, [login, Conditions, 10, 10], [Plist,Plist,Plist]),
  meck:expect(sumo, find_by, [login, Conditions, username, 10, 10], [Plist,Plist,Plist])
  .

unload_mocks(_) ->
  meck:unload(login),
  meck:unload(fakemod),
  meck:unload(sumo).

%%% Fixtures

login_schema() ->
  [ #field{name = username, type = string},
    #field{name = password, type = string},
    #field{name = password_confirm, type = string},
    #modifier{type = before_validate, func = fun fakemod:before_validate/2},
    #modifier{type = after_validate, func = fun fakemod:after_validate/2},
    #modifier{type = before_commit, func = fun fakemod:before_commit/2},
    #modifier{type = before_delete, func = fun fakemod:before_delete/2},
    #modifier{type = before_delete_by, func = fun fakemod:before_delete_by/2},
    #modifier{type = after_read, func = fun fakemod:after_read/2}
  ].

login() ->
  [ {username, "spiegela"},
    {password, "12345"},
    {password_confirm, "12345"}
  ].

%%% Properties

field_returns_record() ->
  ?FORALL({Name, Type}, {field_name(), field_type()},
          begin
            dohyo:field(Name, Type) =:= #field{name = Name, type = Type}
          end).

field_with_args_returns_record() ->
  ?FORALL({Name, Type, Attrs}, {field_name(), field_type(), field_attrs()},
          begin
            dohyo:field(Name, Type, Attrs) =:=
              #field{name = Name, type = Type, attrs = Attrs}
          end).

has_many_returns_record() ->
  ?FORALL(Name, association_name(),
          begin
            dohyo:has_many(Name) =:=
              #association{type = has_many, name = Name, schema = Name}
          end).

has_many_with_opts_returns_record() ->
  ?FORALL({Name, Opts}, {association_name(), proplists:proplist()},
          begin
            dohyo:has_many(Name, Opts) =:=
              #association{type = has_many, name = Name, schema = Name,
                           options = Opts}
          end).

belongs_to_returns_record() ->
  ?FORALL(Name, association_name(),
          begin
            dohyo:belongs_to(Name) =:=
              #association{type = belongs_to, name = Name, schema = Name}
          end).

belongs_to_with_opts_returns_record() ->
  ?FORALL({Name, Opts}, {association_name(), proplists:proplist()},
          begin
            dohyo:belongs_to(Name, Opts) =:=
              #association{type = belongs_to, name = Name, schema = Name,
                           options = Opts}
          end).

validate_returns_record() ->
  ?FORALL({Type, Field}, {validation_type(), field_name()},
          begin
            dohyo:validate(Type, Field) =:=
              #validation{type = Type, field = Field}
          end).

validate_with_args_returns_record() ->
  ?FORALL({Type, Field, Args},
          {validation_type(), field_name(), validation_args()},
          begin
            dohyo:validate(Type, Field, Args) =:=
              #validation{type = Type, field = Field, args = Args}
          end).

before_validate_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:before_validate(Fun) =:=
              #modifier{type = before_validate, func = Fun}
          end).

after_validate_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:after_validate(Fun) =:=
              #modifier{type = after_validate, func = Fun}
          end).

before_commit_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:before_commit(Fun) =:=
              #modifier{type = before_commit, func = Fun}
          end).

before_delete_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:before_delete(Fun) =:=
              #modifier{type = before_delete, func = Fun}
          end).

before_delete_by_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:before_delete_by(Fun) =:=
              #modifier{type = before_delete_by, func = Fun}
          end).

after_read_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:after_read(Fun) =:=
              #modifier{type = after_read, func = Fun}
          end).

on_create_returns_record() ->
  ?FORALL(Fun, hook_fun(),
          begin
            dohyo:on_create(Fun) =:=
              #hook{type = on_create, func = Fun}
          end).

on_update_returns_record() ->
  ?FORALL(Fun, hook_fun(),
          begin
            dohyo:on_update(Fun) =:=
              #hook{type = on_update, func = Fun}
          end).

on_delete_returns_record() ->
  ?FORALL(Fun, hook_fun(),
          begin
            dohyo:on_delete(Fun) =:=
              #hook{type = on_delete, func = Fun}
          end).

on_delete_all_returns_record() ->
  ?FORALL(Fun, hook_fun(),
          begin
            dohyo:on_delete_all(Fun) =:=
              #hook{type = on_delete_all, func = Fun}
          end).

on_schema_create_returns_record() ->
  ?FORALL(Fun, hook_fun(),
          begin
            dohyo:on_schema_create(Fun) =:=
              #hook{type = on_schema_create, func = Fun}
          end).
