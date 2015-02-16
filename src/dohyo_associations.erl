-module(dohyo_associations).

-include_lib("dohyo.hrl").

-export[fetch/3, key/3].

-spec fetch(module(), #association{}, proplists:proplist()) ->
  [sumo:user_doc()] | sumo:user_doc() | undefined.
fetch(Module, #association{type = has_many}=Assoc, Plist) ->
  sumo:find_by(schema(Assoc), [conditions(Plist, Module, Assoc)]);
fetch(Module, #association{type = belongs_to}=Assoc, Plist) ->
  case sumo:find_one(schema(Assoc), [conditions(Plist, Module, Assoc)]) of
    not_found -> undefined;
    Element -> Element
  end.

-spec key(key_type(), module(), #association{}) -> atom().
key(KeyType, Module,  #association{options = Opts}=A) when Opts =/= [] ->
  case lists:keyfind(KeyType, 1, Opts) of
    false -> key(KeyType, Module, A#association{options = []});
    {_, Key}  -> Key
  end;
key(local_key, _Module, #association{type = belongs_to, name = Name}) ->
  append_id_to_atom(Name);
key(foreign_key, _Module, #association{type = belongs_to}) ->
  id;
key(local_key, _Module, #association{type = has_many}) ->
  id;
key(foreign_key, Module, #association{type = has_many}) ->
  append_id_to_atom(Module).
  
%% @private
-spec conditions(proplists:proplist(), module(), #association{}) ->
  {atom(), term()}.
conditions(Plist, Module, Assoc) ->
  {_, ID} = lists:keyfind(key(local_key, Module, Assoc), 1, Plist),
  {key(foreign_key, Module, Assoc), ID}.

%% @private
-spec append_id_to_atom(association_name()) -> atom().
append_id_to_atom(Assoc) ->
  list_to_atom(lists:concat([atom_to_list(Assoc), "_id"])).

%% @private
-spec schema(#association{}) -> atom().
schema(#association{name = Name, options = []}) ->
  Name;
schema(#association{name = Name, options = Opts}) ->
  case lists:keyfind(schema, 1, Opts) of
    false       -> Name;
    {_, Schema} -> Schema
  end.
