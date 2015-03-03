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

-module(dohyo_associations).

-include_lib("dohyo.hrl").

-export[fetch/3, fetch_ids/3, key/3, lookup/1, lookup/2].

-spec lookup(sumo:schema_name()) -> [#association{}].
lookup(Module) ->
  Fun = fun(#association{}) -> true; (_Entity) -> false end,
  lists:filter(Fun, Module:schema()).

-spec lookup(sumo:schema_name(), association_name()) ->
  #association{} | not_found.
lookup(Module, Name1) ->
  Fun = fun(#association{name = Name}) when Name =:= Name1 -> false;
           (_Entity) -> true
        end,
  case lists:dropwhile(Fun, Module:schema()) of
    []     -> not_found;
    [X|_T] -> X
  end.

-spec fetch_ids(module(), #association{}, proplists:proplist()) ->
  [string() | pos_integer()] | undefined.
fetch_ids(Module, #association{type = has_many, schema = Schema}=Assoc, Plist) ->
  select_ids(Schema, fetch(Module, Assoc, Plist));
fetch_ids(Module, #association{type = belongs_to}=Assoc, Plist) ->
  {_, Id} = lists:keyfind(key(local_key, Module, Assoc), 1, Plist), Id.

-spec fetch(module(), #association{}, proplists:proplist()) ->
  [sumo:user_doc()] | sumo:user_doc() | undefined.
fetch(Module, #association{schema = Schema, type = has_many}=Assoc, Plist) ->
  sumo:find_by(Schema, [conditions(Plist, Module, Assoc)]);
fetch(Module, #association{schema = Schema, type = belongs_to}=Assoc, Plist) ->
  case sumo:find_one(Schema, [conditions(Plist, Module, Assoc)]) of
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
-spec select_ids(module(), [proplists:proplist()]) ->
  [string() | pos_integer()].
select_ids(Module, Entities) ->
  Fun = fun(Entity) ->
          {_, ID} =
            lists:keyfind(sumo_internal:id_field_name(Module), 1, Entity),
          ID
        end,
  lists:map(Fun, Entities).
