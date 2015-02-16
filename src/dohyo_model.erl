-module(dohyo_model).

-export([sumo_schema/1]).

-include_lib("dohyo.hrl").

-callback schema() -> [schema_entry()].
-callback sumo_schema() -> sumo:schema().
-callback sumo_wakeup(sumo:doc()) -> sumo:user_doc().
-callback sumo_sleep(sumo:user_doc()) -> sumo:doc().

%% @doc
%% Convert a dohyo schema to a sumo schema making it available to the sumo.
%% @end
-spec sumo_schema(sumo:schema_name()) -> sumo:schema().
sumo_schema(Module) ->
  sumo:new_schema(Module, sumo_fields(Module:schema())).

%% @private
-spec sumo_fields(schema()) -> [sumo:field()].
sumo_fields(Schema) -> lists:filtermap(fun sumo_field/1, Schema).

%% @private
-spec sumo_field(schema_entry()) -> {true, sumo:field()} | false.
sumo_field(#field{name = Name, type = Type, attrs = Attrs}) ->
  {true, sumo:new_field(Name, Type, Attrs)};
sumo_field(#association{type = belongs_to, options = Opts}=Assoc) ->
  Name = dohyo_associations:key(local_key, undefind, Assoc),
  {true, sumo:new_field(Name, integer, field_attrs(Opts))};
sumo_field(_Entry) ->
  false.

%% @private
-spec field_attrs(proplists:proplist()) -> [atom()].
field_attrs(Opts) ->
  case lists:keyfind(attrs, 1, Opts) of
    false -> [];
    Attrs -> Attrs
  end.
