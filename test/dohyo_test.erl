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

-module(dohyo_test).

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
    },
    { "fetches has_many association",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_has_many_association/0
       }
    },
    { "fetches belongs_to association",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_belongs_to_association/0
       }
    },
    { "fetches has_many ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_has_many_ids/0
       }
    },
    { "fetches belongs_to ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_belongs_to_ids/0
       }
    },
    { "fetches all associations",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_all_associations/0
       }
    },
    { "fetches all associations ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun fetches_all_association_ids/0
       }
    },
    { "embeds has_many association",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_has_many_association/0
       }
    },
    { "embeds belongs_to association",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_belongs_to_association/0
       }
    },
    { "embeds has_many ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_has_many_ids/0
       }
    },
    { "embeds belongs_to ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_belongs_to_ids/0
       }
    },
    { "embeds all associations",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_all_associations/0
       }
    },
    { "embeds all associations ids",
       { setup,
         fun setup_assoc_mocks/0,
         fun unload_assoc_mocks/1,
         fun embeds_all_association_ids/0
       }
    }
  ].

%%% Property Tests

property_test_() ->
  [ ?_assertEqual(true, ?_proper_passes(field_returns_record())),
    ?_assertEqual(true, ?_proper_passes(field_with_args_returns_record())),
    ?_assertEqual(true, ?_proper_passes(has_many_returns_record())),
    ?_assertEqual(true, ?_proper_passes(has_many_with_opts_returns_record())),
    ?_assertEqual(true, ?_proper_passes(belongs_to_returns_record())),
    ?_assertEqual(true, ?_proper_passes(belongs_to_with_opts_returns_record())),
    ?_assertEqual(true, ?_proper_passes(validate_returns_record())),
    ?_assertEqual(true, ?_proper_passes(validate_with_args_returns_record())),
    ?_assertEqual(true, ?_proper_passes(before_validate_returns_record())),
    ?_assertEqual(true, ?_proper_passes(after_validate_returns_record())),
    ?_assertEqual(true, ?_proper_passes(before_commit_returns_record())),
    ?_assertEqual(true, ?_proper_passes(before_delete_returns_record())),
    ?_assertEqual(true, ?_proper_passes(before_delete_by_returns_record())),
    ?_assertEqual(true, ?_proper_passes(after_read_returns_record())),
    ?_assertEqual(true, ?_proper_passes(after_read_many_returns_record())),
    ?_assertEqual(true, ?_proper_passes(on_create_returns_record())),
    ?_assertEqual(true, ?_proper_passes(on_update_returns_record())),
    ?_assertEqual(true, ?_proper_passes(on_delete_returns_record())),
    ?_assertEqual(true, ?_proper_passes(on_delete_all_returns_record())),
    ?_assertEqual(true, ?_proper_passes(on_schema_create_returns_record())),
    ?_assertEqual(true, ?_proper_passes(validate_by_fun_returns_record())),
    ?_assertEqual( true,
                   ?_proper_passes(validate_field_by_fun_returns_record())
                 )
  ].

%%% Unit Tests

wrap_sumo_persist() ->
  State = [],
  Plist = dohyo:persist(login, login(), State),
  [ ?assertEqual(login(), Plist),
    ?assert(meck:validate(login)),
    ?assertEqual(7, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(1, meck:num_calls(fakemod, before_validate, [Plist, State])),
    ?assertEqual(1, meck:num_calls(fakemod, after_validate, [Plist, State])),
    ?assertEqual(1, meck:num_calls(fakemod, before_commit, [Plist, State])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, persist, [login, Plist])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, State]))
  ].

wrap_sumo_delete() ->
  State = [],
  Id = 5,
  true = dohyo:delete(login, Id, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(1, meck:num_calls(fakemod, before_delete, [Id, State])),
    ?assertEqual(1, meck:num_calls(sumo, delete, [login, Id]))
  ].

wrap_sumo_delete_by() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  11 = dohyo:delete_by(login, Conditions, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(2, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(1, meck:num_calls( fakemod,
                                    before_delete_by,
                                    [Conditions, State]
                                  )),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, delete_by, [login, Conditions]))
  ].

wrap_sumo_delete_all() ->
  State = [],
  22 = dohyo:delete_all(login, State),
  [ ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, delete_all, [login]))
  ].

wrap_sumo_find() ->
  State = [],
  Plist = login(),
  Plist = dohyo:find(login, 5, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(2, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find, [login, 5])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, []]))
  ].

wrap_sumo_find_one() ->
  State = [],
  Plist = login(),
  Conditions = [{username, "spiegela"}],
  Plist = dohyo:find_one(login, Conditions, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(3, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_one, [login, Conditions])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(1, meck:num_calls(fakemod, after_read, [Plist, []]))
  ].

wrap_sumo_find_all_2() ->
  State = [],
  Plists = [login(), login(), login()],
  Plists = dohyo:find_all(login, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(7, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_all, [login])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(3, meck:num_calls(fakemod, after_read, [login(), []])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read_many, [Plists, []]))
  ].

wrap_sumo_find_all_4() ->
  State = [],
  Plists = [login(), login(), login()],
  Plists = dohyo:find_all(login, username, 10, 10, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(7, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_all, [login, username, 10, 10])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(3, meck:num_calls(fakemod, after_read, [login(), []])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read_many, [Plists, []]))
  ].

wrap_sumo_find_by_2() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists = [login(), login(), login()],
  Plists = dohyo:find_by(login, Conditions, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(8, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [login, Conditions])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(3, meck:num_calls(fakemod, after_read, [login(), []])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read_many, [Plists, []]))
  ].

wrap_sumo_find_by_4() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists = [login(), login(), login()],
  Plists = dohyo:find_by(login, Conditions, 10, 10, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(8, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [login, Conditions, 10, 10])),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(3, meck:num_calls(fakemod, after_read, [login(), []])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read_many, [Plists, []]))
  ].

wrap_sumo_find_by_5() ->
  State = [],
  Conditions = [{username, "spiegela"}],
  Plists0 = [login(), login(), login()],
  Plists1 = dohyo:find_by(login, Conditions, username, 10, 10, State),
  [ ?assert(meck:validate(login)),
    ?assertEqual(8, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual( 1,
                  meck:num_calls( sumo,
                                  find_by,
                                  [login, Conditions, username, 10, 10]
                                )
                ),
    ?assert(meck:validate(fakemod)),
    ?assertEqual(3, meck:num_calls(fakemod, after_read, [login(), []])),
    ?assertEqual(1, meck:num_calls(fakemod, after_read_many, [Plists0, []])),
    ?assertEqual(Plists0, Plists1)
  ].

fetches_has_many_association() ->
  Login = login(),
  Roles = dohyo:association(login, roles, Login),
  Conditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, Conditions])),
    ?assertEqual({roles, roles()}, Roles)
  ].

fetches_belongs_to_association() ->
  Login = login(),
  Account = dohyo:association(login, account, Login),
  Conditions = [{'account.id', 12}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_one, [account, Conditions])),
    ?assertEqual({account, account()}, Account)
  ].

fetches_has_many_ids() ->
  Login = login(),
  RolesIds = dohyo:association_ids(login, roles, Login),
  Conditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, Conditions])),
    ?assertEqual({roles, [1, 2, 3]}, RolesIds)
  ].

fetches_belongs_to_ids() ->
  Login = login(),
  Account = dohyo:association_ids(login, account, Login),
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual({account, 12}, Account)
  ].

fetches_all_associations() ->
  Login = login(),
  Assocs = dohyo:all_associations(login, Login),
  RoleConditions = [{'role.login_id', 5}],
  AcctConditions = [{'account.id', 12}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, RoleConditions])),
    ?assertEqual(1, meck:num_calls(sumo, find_one, [account, AcctConditions])),
    ?assertEqual([{roles, roles()}, {account, account()}], Assocs)
  ].

fetches_all_association_ids() ->
  Login = login(),
  AssocIds = dohyo:all_association_ids(login, Login),
  RoleConditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, RoleConditions])),
    ?assertEqual([{roles, [1, 2, 3]}, {account, 12}], AssocIds)
  ].

embeds_has_many_association() ->
  Login = login(),
  Roles = roles(),
  Login2 = dohyo:associate(login, roles, Login),
  Conditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, Conditions])),
    ?assertEqual(Login ++ [{roles, Roles}], Login2)
  ].

embeds_belongs_to_association() ->
  Login = login(),
  Account = account(),
  Login2 = dohyo:associate(login, account, Login),
  Conditions = [{'account.id', 12}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_one, [account, Conditions])),
    ?assertEqual(Login ++ [{account, Account}], Login2)
  ].

embeds_has_many_ids() ->
  Login = login(),
  Login2 = dohyo:associate_ids(login, roles, Login),
  Conditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, Conditions])),
    ?assertEqual(Login ++ [{roles, [1, 2, 3]}], Login2)
  ].

embeds_belongs_to_ids() ->
  Login = login(),
  Login2 = dohyo:associate_ids(login, account, Login),
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(Login ++ [{account, 12}], Login2)
  ].

embeds_all_associations() ->
  Login = login(),
  Login2 = dohyo:associate_all(login, Login),
  RoleConditions = [{'role.login_id', 5}],
  AcctConditions = [{'account.id', 12}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, RoleConditions])),
    ?assertEqual(1, meck:num_calls(sumo, find_one, [account, AcctConditions])),
    ?assertEqual(Login ++ [{roles, roles()}, {account, account()}], Login2)
  ].

embeds_all_association_ids() ->
  Login = login(),
  Login2 = dohyo:associate_all_ids(login, Login),
  RoleConditions = [{'role.login_id', 5}],
  [ ?assert(meck:validate(login)),
    ?assertEqual(1, meck:num_calls(login, schema, [])),
    ?assert(meck:validate(sumo)),
    ?assertEqual(1, meck:num_calls(sumo, find_by, [role, RoleConditions])),
    ?assertEqual(Login ++ [{roles, [1, 2, 3]}, {account, 12}], Login2)
  ].

%%% Setup Functions

setup_mocks() ->
  Plist = login(),
  Plists = [login(), login(), login()],
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
  meck:expect(fakemod, after_read_many, [Plists, []], Plists),
  meck:expect(sumo, persist, [login, Plist], Plist),
  meck:expect(sumo, delete, [login, Id], true),
  meck:expect(sumo, delete_by, [login, Conditions], 11),
  meck:expect(sumo, delete_all, [login], 22),
  meck:expect(sumo, find, [login, 5], Plist),
  meck:expect(sumo, find_one, [login, Conditions], Plist),
  meck:expect(sumo, find_all, [login], [Plist, Plist, Plist]),
  meck:expect(sumo, find_all, [login, username, 10, 10], Plists),
  meck:expect(sumo, find_by, [login, Conditions], Plists),
  meck:expect(sumo, find_by, [login, Conditions, 10, 10], Plists),
  meck:expect(sumo, find_by, [login, Conditions, username, 10, 10], Plists).  

setup_assoc_mocks() ->
  meck:new(login, [non_strict]),
  meck:expect(login, schema, [], login_schema()),
  meck:expect(sumo, find_one, [account, [{'account.id', 12}]], account()),
  meck:expect(sumo, find_by, [role, [{'role.login_id', 5}]], roles()),
  meck:expect(sumo_internal, id_field_name, ['_'], id).

unload_mocks(_) ->
  meck:unload(login),
  meck:unload(fakemod),
  meck:unload(sumo).

unload_assoc_mocks(_) ->
  meck:unload(login),
  meck:unload(sumo),
  meck:unload(sumo_internal).

%%% Fixtures

login_schema() ->
  [ #field{name = username, type = string},
    #field{name = password, type = string},
    #field{name = password_confirm, type = string},
    #association{type = has_many, name = roles, options = #{schema => role}},
    #association{type = belongs_to, name = account},
    #modifier{type = before_validate, func = fun fakemod:before_validate/2},
    #modifier{type = after_validate, func = fun fakemod:after_validate/2},
    #modifier{type = before_commit, func = fun fakemod:before_commit/2},
    #modifier{type = before_delete, func = fun fakemod:before_delete/2},
    #modifier{type = before_delete_by, func = fun fakemod:before_delete_by/2},
    #modifier{type = after_read, func = fun fakemod:after_read/2},
    #modifier{type = after_read_many, func = fun fakemod:after_read_many/2}
  ].

login() ->
  [ {id, 5},
    {username, "spiegela"},
    {password, "12345"},
    {password_confirm, "12345"},
    {account_id, 12}
  ].

roles() ->
  [ [ {id, 1},
      {login_id, 5},
      {name, "admin"}
    ],
    [ {id, 2},
      {login_id, 5},
      {name, "user"}
    ],
    [ {id, 3},
      {login_id, 5},
      {name, "operator"}
    ]
  ].

account() ->
  [ {id, 12},
    {name, "Acme Corp"}
  ].

%%% Properties

field_returns_record() ->
  ?FORALL({Name, Type}, {field_name(), field_type()},
          begin
            dohyo:field(Name, Type) =:= #field{ name = Name,
                                                type = Type,
                                                options = #{}
                                              }
          end).

field_with_args_returns_record() ->
  ?FORALL({Name, Type, Options},
          {field_name(), field_type(), #{ alias => atom() }
          },
          begin
            dohyo:field(Name, Type, Options) =:=
              #field{name = Name, type = Type, options = Options}
          end).

has_many_returns_record() ->
  ?FORALL(Name, association_name(),
          begin
            meck:expect(sumo_internal, id_field_name, [Name], id),
            dohyo:has_many(Name) =:=
              #association{ type = has_many, name = Name }
          end).

has_many_with_opts_returns_record() ->
  ?FORALL({Name, Opts}, {association_name(), #{}},
          begin
            dohyo:has_many(Name, Opts) =:=
              #association{type = has_many, name = Name, options = Opts}
          end).

belongs_to_returns_record() ->
  ?FORALL(Name, association_name(),
          begin
            dohyo:belongs_to(Name) =:=
              #association{type = belongs_to, name = Name}
          end).

belongs_to_with_opts_returns_record() ->
  ?FORALL({Name, Opts}, {association_name(), #{}},
          begin
            dohyo:belongs_to(Name, Opts) =:=
              #association{type = belongs_to, name = Name, options = Opts}
          end).

validate_returns_record() ->
  ?FORALL({Type, Field}, {validation_type(), field_name()},
          begin
            dohyo:validate(Field, Type) =:=
              #validation{type = Type, field = Field}
          end).

validate_with_args_returns_record() ->
  ?FORALL({Type, Field, Args},
          {validation_type(), field_name(), validation_args()},
          begin
            dohyo:validate(Field, Type, Args) =:=
              #validation{type = Type, field = Field, args = Args}
          end).

validate_by_fun_returns_record() ->
  ?FORALL(Fun, doc_validator_fun(),
          begin
            dohyo:validate_by(Fun) =:=
              #validation{type = by, args = Fun}
          end).

validate_field_by_fun_returns_record() ->
  ?FORALL({Field, Fun}, {field_name(), field_validator_fun()},
          begin
            dohyo:validate(Field, by, Fun) =:=
              #validation{type = by, field = Field, args = Fun}
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

after_read_many_returns_record() ->
  ?FORALL(Fun, modifier_fun(),
          begin
            dohyo:after_read_many(Fun) =:=
              #modifier{type = after_read_many, func = Fun}
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
