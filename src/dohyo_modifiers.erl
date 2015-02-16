-module(dohyo_modifiers).

-include_lib("dohyo.hrl").

-export([before_validate/3, after_validate/3, before_commit/3, before_delete/3,
         after_read/3]).

%% @doc
%% Run before_validate modifiers against property list in order of
%% definition.  Returns a property-list.
%% @end
-spec before_validate(sumo:schema_name(), sumo:user_doc(), term()) ->
  sumo:user_doc().
before_validate(Module, Plist, State) ->
  run_modifier(before_validate, Module, Plist, State).

%% @doc
%% Run after_validate modifiers against property list in order of
%% definition.  Returns a property-list.
%% @end
-spec after_validate(sumo:schema_name(), sumo:user_doc(), term()) ->
  proplists:proplist().
after_validate(Module, Plist, State) ->
  run_modifier(after_validate, Module, Plist, State).

%% @doc
%% Run before_commit modifiers against property list in order of
%% definition.  Returns a property-list.
%% @end
-spec before_commit(sumo:schema_name(), sumo:user_doc(), term()) ->
  proplists:proplist().
before_commit(Module, Plist, State) ->
  run_modifier(before_commit, Module, Plist, State).

%% @doc
%% Run before_delete modifiers against property list in order of
%% definition.  Returns a property-list.
%% @end
-spec before_delete(sumo:schema_name(), sumo:user_doc(), term()) ->
  proplists:proplist().
before_delete(Module, Plist, State) ->
  run_modifier(before_delete, Module, Plist, State).

%% @doc
%% Run after_read modifiers against property list in order of
%% definition.  Returns a property-list.
%% @end
-spec after_read(sumo:schema_name(), sumo:user_doc(), term()) ->
  proplists:proplist().
after_read(Module, Plist, State) ->
  run_modifier(after_read, Module, Plist, State).

%% @private
-spec run_modifier(
        modifier_type(),
        sumo:schema_name(),
        sumo:user_doc(),
        term()
       ) ->
  sumo:user_doc().
run_modifier(Trigger, Module, Plist, State) ->
  lists:foldl( fun(Fun, Plist0) -> Fun(Plist0, State) end,
               Plist,
               modifiers(Trigger, Module:schema())
             ).

%% @private
-spec modifiers(modifier_type(), schema()) -> [#modifier{}].
modifiers(Trigger, Schema) ->
  lists:filter(fun(#modifier{type = Type}) when Type =:= Trigger -> true;
                  (_Entry) -> false
               end, Schema).
