%%% @doc Main module for dohyo.
%%% 
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

-module(dohyo).

-include_lib("dohyo.hrl").

%%%
%%% API for CRUD operations
%%%
-export([persist/3, delete/2, delete_by/2, delete_all/1]).
-export([find/3, find_all/2, find_all/5]).
-export([find_by/3, find_by/5, find_by/6, find_one/3]).

%%%
%%% API for association fetching
%%%
-export([associate/3, associate_all/2, association/3, all_associations/2,
         associate_ids/3, association_ids/3, associate_all_ids/2,
         all_association_ids/2]).

%%%
%%% API for model definition
%%%
-export([field/2, field/3]).
-export([has_many/1, has_many/2, belongs_to/1, belongs_to/2]).
-export([validate/2, validate/3]).
-export([before_validate/1, after_validate/1, before_commit/1, before_delete/1,
         after_read/1]).
-export([on_create/1, on_update/1, on_delete/1, on_delete_all/1,
         on_schema_create/1]).

%% @doc Create or update a given doc having run validations & modifiers.
-spec persist(sumo:schema_name(), sumo:user_doc(), term()) -> sumo:user_doc().
persist(Module, Plist, State) ->
  Plist1 = dohyo_modifiers:before_validate(Module, Plist, State),
  ok = dohyo_validations:validate(Module, Plist1),
  Plist2 = dohyo_modifiers:after_validate(Module, Plist1, State),
  Plist3 = dohyo_modifiers:before_commit(Module, Plist2, State),
  sumo:persist(Module, Plist3).

%% @doc
%% Alias sumo:delete/2 to keep consistent API.  Not validation or modifier is
%% needed.
%% @equiv sumo:delete(DocName, Id).
%% @end
-spec delete(sumo:schema_name(), sumo:user_doc()) -> boolean().
delete(Module, Id) -> sumo:delete(Module, Id).

%% @doc
%% Alias sumo:delete_by/2 to keep consistent API.  Not validation or modifier
%% is needed.
%% @equiv sumo:delete_by(DocName, Condistions).
%% @end
-spec delete_by(sumo:schema_name(), sumo:conditions()) -> non_neg_integer().
delete_by(Module, Conditions) -> sumo:delete_by(Module, Conditions).

%% @doc
%% Alias sumo:delete_all/1 to keep consistent API.  Not validation or
%% modifier is needed.
%% @equiv sumo:delete_all(DocName).
%% @end
-spec delete_all(sumo:schema_name()) -> non_neg_integer().
delete_all(Module) -> sumo:delete_all(Module).

%% @doc Returns the doc identified by Id having run modifier.
-spec find(sumo:schema_name(), sumo:field_value(), term()) -> sumo:user_doc().
find(Module, Id, State) ->
  Plist = sumo:find(Module, Id),
  dohyo_modifiers:after_read(Module, Plist, State).

%% @doc
%% Returns 1 doc that matches the given Conditions having run after_read
%% modifier.
%% @end
-spec find_one(sumo:schema_name(), sumo:conditions(), term()) ->
  sumo:user_doc().
find_one(Module, Conditions, State) ->
  Plist = sumo:find_one(Module, Conditions),
  dohyo_modifiers:after_read(Module, Plist, State).

%% @doc
%% Return all the docs from the given store having run the after_read
%% modifier.
%% @end
-spec find_all(sumo:schema_name(), term()) -> [sumo:user_doc()].
find_all(Module, State) ->
  [ dohyo_modifiers:after_read(Module, Plist, State) ||
     Plist <- sumo:find_all(Module)
  ].

%% @doc
%% Return all the docs from the given store sorted and by offset, having run
%% the after_read modifier.
%% @end
-spec find_all(sumo:schema_name(), sumo:sort(), non_neg_integer(),
               non_neg_integer() ,term()) ->
  [sumo:user_doc()].
find_all(Module, Sort, Limit, Offset, State) ->
  [ dohyo_modifiers:after_read(Module, Plist, State) ||
     Plist <- sumo:find_all(Module, Sort, Limit, Offset)
  ].

%% @doc Returns *all* docs that match Conditions, having run the after_read
%% modifier.
-spec find_by(sumo:schema_name(), sumo:conditions(), term()) ->
  [sumo:user_doc()].
find_by(Module, Conditions, State) ->
  [ dohyo_modifiers:after_read(Module, Plist, State) ||
     Plist <- sumo:find_by(Module, Conditions)
  ].

%% @doc Returns Limit number of docs that match Conditions, starting at
%% offset Offset, having run the after_read modifier.
-spec find_by(sumo:schema_name(), sumo:conditions(), non_neg_integer(),
              non_neg_integer(), term()) ->
  [sumo:user_doc()].
find_by(Module, Conditions, Limit, Offset, State) ->
  [ dohyo_modifiers:after_read(Module, Plist, State) ||
     Plist <- sumo:find_by(Module, Conditions, Limit, Offset)
  ].

%% @doc Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
-spec find_by(sumo:schema_name(), sumo:conditions(), sumo:sort(),
              non_neg_integer(), non_neg_integer(), term()) ->
  [sumo:user_doc()].
find_by(Module, Conditions, Sort, Limit, Offset, State) ->
  [ dohyo_modifiers:after_read(Module, Plist, State) ||
     Plist <- sumo:find_by(Module, Conditions, Sort, Limit, Offset)
  ].

%% @equiv field(Name, Type, []).
-spec field(field_name(), field_type()) -> #field{}.
field(Name, Type) -> #field{name = Name, type = Type}.

%% @doc Returns a field record to be used as part of a schema
-spec field(field_name(), field_type(), field_attrs()) -> #field{}.
field(Name, Type, Attrs) -> #field{name = Name, type = Type, attrs = Attrs}.

%% @equiv has_many(Name, []).
-spec has_many(association_name()) -> #association{}.
has_many(Name) -> #association{schema = Name, type = has_many, name = Name}.

%% @doc
%% Returns an association record of type has_many to be used as part of a
%% schema
%% @end
-spec has_many(association_name(), proplists:proplist()) ->
  #association{type :: has_many, options :: proplists:proplist()}.
has_many(Name, Opts) ->
  case lists:keytake(schema, 1, Opts) of
    {value, {_, Module}, Opts1} ->
      #association{type = has_many, name = Name, schema = Module,
                   options = Opts1};
    false ->
      #association{type = has_many, name = Name, schema = Name,
                   options = Opts}
  end.

%% @equiv belongs_to(Name, []).
-spec belongs_to(association_name()) ->
  #association{type :: belongs_to, options :: proplists:proplist()}.
belongs_to(Name) -> #association{schema = Name, type = belongs_to,
                                 name = Name}.

%% @doc
%% Returns an association record of type belongs_to to be used as part of a
%% schema
%% @end
-spec belongs_to(association_name(), proplists:proplist()) ->
  #association{type :: belongs_to, options :: proplists:proplist()}.
belongs_to(Name, Opts) ->
  case lists:keytake(schema, 1, Opts) of
    {value, {_, Module}, Opts1} ->
      #association{type = belongs_to, name = Name, schema = Module,
                   options = Opts1};
    false ->
      #association{type = belongs_to, name = Name, schema = Name,
                   options = Opts}
  end.

%% @equiv validate(Type, Name, []).
-spec validate(validation_type(), field_name()) -> #validation{}.
validate(Type, Field) -> #validation{type = Type, field = Field}.

%% @doc Returns a validation record to be used as part of a schema
-spec validate(validation_type(), field_name(), validation_args()) ->
        #validation{}.
validate(Type, Field, Args) -> #validation{type = Type, field = Field,
                                           args = Args}.

%% @doc
%% Returns a modifier record of type before_validate to be used a part of a
%% schema
%% @end
-spec before_validate(modifier_fun()) -> #modifier{}.
before_validate(Fun) -> #modifier{type = before_validate, func = Fun}.

%% @doc
%% Returns a modifier record of type after_validate to be used a part of a
%% schema
%% @end
-spec after_validate(modifier_fun()) -> #modifier{}.
after_validate(Fun) -> #modifier{type = after_validate, func = Fun}.

%% @doc
%% Returns a modifier record of type before_commit to be used a part of a
%% schema
%% @end
-spec before_commit(modifier_fun()) -> #modifier{}.
before_commit(Fun) -> #modifier{type = before_commit, func = Fun}.

%% @doc
%% Returns a modifier record of type before_delete to be used a part of a
%% schema
%% @end
-spec before_delete(modifier_fun()) -> #modifier{}.
before_delete(Fun) -> #modifier{type = before_delete, func = Fun}.

%% @doc
%% Returns a modifier record of type after_read to be used a part of a
%% schema
%% @end
-spec after_read(modifier_fun()) -> #modifier{}.
after_read(Fun) -> #modifier{type = after_read, func = Fun}.

%% @doc
%% Returns a hook record of type on_create to be used a part of a
%% schema
%% @end
-spec on_create(hook_fun()) -> #hook{}.
on_create(Fun) -> #hook{type = on_create, func = Fun}.

%% @doc
%% Returns a hook record of type on_update to be used a part of a
%% schema
%% @end
-spec on_update(hook_fun()) -> #hook{}.
on_update(Fun) -> #hook{type = on_update, func = Fun}.

%% @doc
%% Returns a hook record of type on_delete to be used a part of a
%% schema
%% @end
-spec on_delete(hook_fun()) -> #hook{}.
on_delete(Fun) -> #hook{type = on_delete, func = Fun}.

%% @doc
%% Returns a hook record of type on_delete_all to be used a part of a
%% schema
%% @end
-spec on_delete_all(hook_fun()) -> #hook{}.
on_delete_all(Fun) -> #hook{type = on_delete_all, func = Fun}.

%% @doc
%% Returns a hook record of type on_schema_create to be used a part of a
%% schema
%% @end
-spec on_schema_create(hook_fun()) -> #hook{}.
on_schema_create(Fun) -> #hook{type = on_schema_create, func = Fun}.

%% @doc
%% Returns a new property-list with the requested association results
%% inserted.
%% @end
-spec associate(sumo:schema_name(), association_name(), sumo:user_doc()) ->
  sumo:user_doc().
associate(Module, Assoc, Plist) ->
  store_association( lists:keyfind(Assoc, 4, Module:associations()),
                     {Module, Plist}
                   ).

%% @doc Returns a new property-list with the requested associated ids inserted.
-spec associate_ids(sumo:schema_name(), association_name(), sumo:user_doc()) ->
  sumo:user_doc().
associate_ids(Module, Assoc, Plist) ->
  store_ids( lists:keyfind(Assoc, 4, Module:associations()),
                     {Module, Plist}
                   ).

%% @doc
%% Returns a new property-list with all defined associations' results for the
%% schema inserted.
%% @end
-spec associate_all(sumo:schema_name(), sumo:user_doc()) -> sumo:user_doc().
associate_all(Module, Plist) ->
  lists:foldl(fun store_association/2, {Module, Plist}, Module:assocations()).

%% @doc
%% Returns a new property-list with all defined associations' ids for the
%% schema inserted.
%% @end
-spec associate_all_ids(sumo:schema_name(), sumo:user_doc()) -> sumo:user_doc().
associate_all_ids(Module, Plist) ->
  lists:foldl(fun store_ids/2, {Module, Plist}, Module:assocations()).

%% @doc
%% Returns a property (tuple) with the requested association results.
%% @end
-spec association(sumo:schema_name(), association_name(), sumo:user_doc()) ->
  [sumo:user_doc()] | sumo:user_doc().
association(Module, AssocName, Plist) ->
  Assoc = lists:keyfind(AssocName, 4, Module:associations()),
  {AssocName, dohyo_associations:fetch(Module, Assoc, Plist)}.

%% @doc
%% Returns a property (tuple) with the requested association results.
%% @end
-spec association_ids(
        sumo:schema_name(),
        association_name(),
        sumo:user_doc()
      ) ->
  [sumo:user_doc()] | sumo:user_doc().
association_ids(Module, AssocName, Plist) ->
  Assoc = lists:keyfind(AssocName, 4, Module:associations()),
  {AssocName, dohyo_associations:fetch_ids(Module, Assoc, Plist)}.

%% @doc
%% Returns a property (tuple) with all defined associations' results for the
%% schema.
%% @end
-spec all_associations(sumo:schema_name(), sumo:user_doc()) -> sumo:user_doc().
all_associations(Module, Plist) ->
  lists:map(fun(#association{name = Name}=Assoc) ->
              {Name, dohyo_associations:fetch(Module, Assoc, Plist)}
            end, Module:associations()).

%% @doc
%% Returns a property (tuple) with all defined associations' results for the
%% schema.
%% @end
-spec all_association_ids(sumo:schema_name(), sumo:user_doc()) ->
  sumo:user_doc().
all_association_ids(Module, Plist) ->
  lists:map(fun(#association{name = Name}=Assoc) ->
              {Name, dohyo_associations:fetch_ids(Module, Assoc, Plist)}
            end, Module:associations()).

%% @private
-spec store_association(
        #association{},
        {sumo:schema_name(), sumo:user_doc()}
       ) ->
  sumo:user_doc().
store_association(#association{name = Name}=Assoc, {Module, Plist}) ->
  { Module,
    lists:keystore( Name, 1, Plist,
                    {Name, dohyo_associations:fetch(Module, Assoc, Plist)}
                  )
  }.

%% @private
-spec store_ids(
        #association{},
        {sumo:schema_name(), sumo:user_doc()}
       ) ->
  sumo:user_doc().
store_ids(#association{name = Name}=Assoc, {Module, Plist}) ->
  { Module,
    lists:keystore( Name, 1, Plist,
                    {Name, dohyo_associations:fetch_ids(Module, Assoc, Plist)}
                  )
  }.
