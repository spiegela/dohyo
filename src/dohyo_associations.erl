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

-module(dohyo_associations).

-include_lib("dohyo.hrl").

-export[fetch/3, fetch_ids/3, lookup/1, lookup/2, local_key/4].

-spec lookup(sumo:schema_name()) -> [association()].
lookup(Module) ->
  lists:filter(fun lookup_filter/1, Module:schema()).

-spec lookup(sumo:schema_name(), association_name()) ->
  association().
lookup(Module, Name1) ->
  Fun = fun(#association{name = Name}) when Name =:= Name1 -> false;
    (_Entity) -> true
  end,
  case lists:dropwhile(Fun, Module:schema()) of
    []     -> error(badarg);
    [X|_T] -> X
  end.

-spec fetch_ids(module(), association(), proplists:proplist()) ->
  [string() | pos_integer()] | undefined.
fetch_ids(Module,
          #association{type = has_many, name = Name, options = Opts}=Assoc,
          Plist
         ) ->
  Schema = schema(has_many, Name, Opts, Plist),
  select_ids(Schema, fetch(Module, Assoc, Plist));
fetch_ids( Module,
           #association{type = belongs_to, name = Name, options = Opts},
           Plist
         ) ->
  LKey    = local_key(belongs_to, Module, Name, Opts),
  {_, Id} = lists:keyfind(LKey, 1, Plist), Id.

-spec fetch( module(), association(), proplists:proplist()) ->
  proplists:proplist().
fetch( Module,
       #association{ type = has_many, options = #{through := Through} }=Assoc,
       Plist
     ) ->
  #association{type = ThrType} = ThrAssoc = lookup(Module, Through),
  {ThrSchema, TarParams} = query_params(ThrType, Module, ThrAssoc, Plist),
  {TarSchema, [{ThrFKey, _}|MaybePoly]} =
    query_params(has_many, ThrSchema, Assoc, Plist),
  ThrLKey = lists:concat([atom_to_list(ThrSchema), ".", unary_key(ThrSchema)]),
  Sql = lists:concat([ "select ", TarSchema, ".* from ", TarSchema,
                       " left join ", ThrSchema, " on ",
                       params_to_conditions([{ThrFKey,ThrLKey}|MaybePoly]),
                       " where ", params_to_conditions(TarParams), ";"
                     ]),
  Pool = sumo_backend_mysql:get_pool(Module),
  sumo_store_mysql_extra:find_by_sql(Sql, TarSchema, {state, Pool});
fetch(Module, #association{type = has_many}=Assoc, Plist) ->
  {Schema, Params} = query_params(has_many, Module, Assoc, Plist),
  sumo:find_by(Schema, Params);
fetch(Module, #association{type = belongs_to}=Assoc, Plist) ->
  {Schema, Params} = query_params(belongs_to, Module, Assoc, Plist),
  case sumo:find_one(Schema, Params) of
    not_found -> undefined;
    Result    -> Result
  end.

-spec query_params(
        association_type(),
        module(),
        association(),
        proplists:proplist()
       ) -> {atom(), proplists:proplist()}.
query_params( has_many,
              Module,
              #association{name = Name, options = Opts},
              Plist
            ) ->
  LKey    = local_key(has_many, Module, Name, Opts),
  {_, ID} = lists:keyfind(LKey, 1, Plist),
  Schema  = schema(has_many, Name, Opts, Plist),
  FKey    = prepend_to_atom( atom_to_list(Schema) ++ ".",
                             foreign_key(has_many, Module, Opts)
                           ),
  {Schema, [{FKey, ID}|maybe_polymorph(Schema, Module, Opts)]};
query_params( belongs_to,
              Module,
              #association{name = Name, options = Opts},
              Plist
            ) ->
  LKey    = local_key(belongs_to, Module, Name, Opts),
  {_, ID} = lists:keyfind(LKey, 1, Plist),
  Schema  = schema(belongs_to, Name, Opts, Plist),
  FKey    = prepend_to_atom( atom_to_list(Schema) ++ ".",
                             foreign_key(belongs_to, Module, Opts)
                           ),
  {Schema, [{FKey, ID}]}.

-spec params_to_conditions(proplists:proplist()) -> string().
params_to_conditions(Params) ->
  string:join(
    lists:map(
      fun({Field, Val}) when is_integer(Val) ->
        string:join([atom_to_list(Field), integer_to_list(Val)], " = ");
          ({Field, Val}) when is_list(Val) ->
        string:join([atom_to_list(Field), Val], " = ");
          ({Field, Val}) when is_atom(Val) ->
        string:join([atom_to_list(Field), atom_to_list(Val)], " = ")
      end, Params
    ),
    " AND "
  ).

-spec maybe_polymorph(module(), module(), association_opts()) ->
  proplists:proplist().
maybe_polymorph(Schema, Module, #{as := PolyKey}) ->
  [ { prepend_to_atom( atom_to_list(Schema) ++ ".",
                       append_to_atom("_type", PolyKey)
                     ),
      Module
    }
  ];
maybe_polymorph(_Schema, _Module, _Opts) ->
  [].

-spec schema(
        association_type(),
        association_name(),
        association_opts(),
        proplists:proplist()
      ) -> atom().
schema(_Type, _Name, #{schema := Schema}, _Plist) ->
  Schema;
schema(belongs_to, Name, #{polymorphic := true}, Plist) ->
  case lists:keyfind(append_to_atom("_type", Name), 1, Plist) of
    false       -> error(badarg);
    {_, Schema} -> list_to_atom(Schema)
  end;
schema(_Type, Name, _Opts, _Plist) ->
  Name.

-spec foreign_key(association_type(), module(), association_opts()) -> atom().
foreign_key(_Type, _Module, #{foreign_key := FKey}) ->
  FKey;
foreign_key(belongs_to, Module, _Opts) ->
  unary_key(Module);
foreign_key(has_many, _Module, #{as := PolyKey}) ->
  multiple_key(PolyKey);
foreign_key(has_many, Module, _Opts) ->
  multiple_key(Module).

-spec local_key(
        association_type(),
        module(),
        association_name(),
        association_opts()
       ) -> atom().
local_key(_Type, _Module, _Name, #{local_key := LKey}) ->
  LKey;
local_key(belongs_to, _Module, Name, _Opts) ->
  multiple_key(Name);
local_key(has_many, Module, _Name, _Opts) ->
  unary_key(Module).

%% @private
-spec multiple_key(atom()) -> atom().
multiple_key(Module) -> append_to_atom("_id", Module).

%% @private
-spec unary_key(atom()) -> atom().
unary_key(Module) -> sumo_internal:id_field_name(Module).

%% @private
-spec append_to_atom(string(), association_name()) -> atom().
append_to_atom(String, Atom) ->
  list_to_atom(lists:concat([atom_to_list(Atom), String])).

%% @private
-spec prepend_to_atom(string(), association_name()) -> atom().
prepend_to_atom(String, Atom) ->
  list_to_atom(lists:concat([String, atom_to_list(Atom)])).

%% @private
-spec select_ids(module(), [proplists:proplist()]) ->
  [string() | pos_integer()].
select_ids(Module, Entities) ->
  IDField = sumo_internal:id_field_name(Module),
  Fun = fun(Entity) ->
          {_, ID} = lists:keyfind(IDField, 1, Entity), ID
        end,
  lists:map(Fun, Entities).

%% @private
-spec lookup_filter(schema_entry()) -> boolean().
lookup_filter(#association{}) -> true;
lookup_filter(_Entity) -> false.
