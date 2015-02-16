-module(dohyo_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/dohyo.hrl").

-include("test_helpers.hrl").

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
    ?_assertEqual(?_proper_passes(after_read_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_create_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_update_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_delete_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_delete_all_returns_record()), true),
    ?_assertEqual(?_proper_passes(on_schema_create_returns_record()), true)
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
