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
