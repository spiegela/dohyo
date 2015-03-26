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

-module(dohyo_model).

-export([sumo_schema/1, alias/2, unalias/2]).

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

%% @doc Replace document field names with aliases defined in schema.
-spec alias(sumo:schema_name(), sumo:user_doc()) -> sumo:user_doc().
alias(Module, Plist) ->
  lists:foldl(fun alias_field/2, Plist, aliased_fields(Module:schema())).

%% @doc Replace document field aliases with names defined in schema.
-spec unalias(sumo:schema_name(), sumo:user_doc()) -> sumo:user_doc().
unalias(Module, Plist) ->
  lists:foldl(fun unalias_field/2, Plist, aliased_fields(Module:schema())).

%% @private
alias_field(#field{name = Name, options = #{alias := Alias}}, Plist) ->
  rekey_field(Name, Alias, Plist).

%% @private
unalias_field(#field{name = Name, options = #{alias := Alias}}, Plist) ->
  rekey_field(Alias, Name, Plist).

%% @private
rekey_field(OldKey, NewKey, Plist) ->
  case lists:keyfind(OldKey, 1, Plist) of
    false      -> Plist;
    {_, Value} -> lists:keyreplace(OldKey, 1, Plist, {NewKey, Value})
  end.

%% @private
-spec aliased_fields(schema()) -> [field()].
aliased_fields(Schema) -> lists:filter(fun aliased_field/1, Schema).

%% @private
-spec aliased_field(field()) -> boolean().
aliased_field(#field{options = #{alias := _Alias}}) -> true;
aliased_field(_SchemaEntry) -> false.

%% @private
-spec sumo_fields(schema()) -> [sumo:field()].
sumo_fields(Schema) -> lists:filtermap(fun sumo_field/1, Schema).

%% @private
-spec sumo_field(schema_entry()) -> {true, sumo:field()} | false.
sumo_field(#field{name = Name, type = Type, options = #{attrs := Attrs}}) ->
  {true, sumo:new_field(Name, Type, Attrs)};
sumo_field(#field{name = Name, type = Type}) ->
  {true, sumo:new_field(Name, Type)};
sumo_field(#association{ type = belongs_to,
                         name = Name,
                         options = #{attrs := Attrs}=Opts
                       }
          ) ->
  Field = dohyo_associations:local_key(belongs_to, undefined, Name, Opts),
  {true, sumo:new_field(Field, integer, Attrs)};
sumo_field(#association{type = belongs_to, name = Name, options = Opts}) ->
  Field = dohyo_associations:local_key(belongs_to, undefined, Name, Opts),
  {true, sumo:new_field(Field, integer)};
sumo_field(_Entry) ->
  false.
