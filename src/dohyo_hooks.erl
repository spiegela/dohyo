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

-module(dohyo_hooks).

-include_lib("dohyo.hrl").

%% Initialization functions
-export([start/0]).

%% Event handler functions
-export([on_schema_create/1, on_delete/2, on_delete_all/1, on_create/2,
         on_update/2]).

%% @doc
%% Register event handlers for any models that contain hooks.
%% @end
-spec start() -> ok.
start() -> lists:foreach(fun start_and_register_handler/1, hook_schemas()).

-spec start_and_register_handler(sumo:schema_name()) -> ok.
start_and_register_handler(Schema) ->
  {ok, Pid} = dohyo_sup:start_hook_handler(Schema),
  register_handler(Pid).

%% @doc
%% Register event handler for a given schema.
%% @end
-spec register_handler(pid()) -> ok.
register_handler(Pid) ->
  Events = case application:get_env(sumo_db, events) of
    undefined     -> [];
    {ok, Events0} -> Events0
  end,
  application:set_env(sumo_db, events, [Pid|Events]).

%% @doc Run on_create hooks with the returned entity. Returns a `ok`.
-spec on_create(sumo:schema_name(), proplists:proplist()) -> ok.
on_create(Module, Entity) -> run_hook_with_arg(on_create, Module, Entity).

%% @doc Run on_update hooks with the returned entity. Returns a `ok`.
-spec on_update(sumo:schema_name(), proplists:proplist()) -> ok.
on_update(Module, Entity) -> run_hook_with_arg(on_update, Module, Entity).

%% @doc Run on_delete hooks with the returned entity. Returns a `ok`.
-spec on_delete(sumo:schema_name(), string() | integer()) -> ok.
on_delete(Module, Id) -> run_hook_with_arg(on_delete, Module, Id).

%% @doc Run on_schema_create hooks with the returned entity. Returns a `ok`.
-spec on_schema_create(sumo:schema_name()) -> ok.
on_schema_create(Module) -> run_hook_without_arg(on_schema_create, Module).

%% @doc Run on_delete_all hooks with the returned entity. Returns a `ok`.
-spec on_delete_all(sumo:schema_name()) -> ok.
on_delete_all(Module) -> run_hook_without_arg(on_delete_all, Module).

%% @doc Returns a list of schemas with hooks defined
%% @private
-spec hook_schemas() -> [sumo:schema_name()].
hook_schemas() -> lists:filtermap(fun schema_with_hooks/1, get_schemas()).

%% @doc
%% Selects doc with any defined hooks.
%% @private
%% @end
-spec schema_with_hooks({sumo:schema_name(), schema()}) ->
  false | {true, sumo:schema_name()}.
schema_with_hooks({_Module, []})          -> false;
schema_with_hooks({ Module, [#hook{}|_T]}) -> {true, Module};
schema_with_hooks({ Module, [_Entry|T]})  -> schema_with_hooks({Module, T}).

%% @doc
%% Returns all the configured schemas.
%% @private
%% @end
-spec get_schemas() -> [{sumo:schema_name(), schema()}].
get_schemas() -> [ {Module, Module:schema()} || Module <- get_docs()].

%% @doc
%% Returns all the configured docs.
%% @private
%% @end
-spec get_docs() -> [sumo:schema_name()].
get_docs() ->
  {ok, Docs} = application:get_env(sumo_db, docs),
  [Module || {Module, _Store} <- Docs].

%% @private
-spec hook_funs(hook_type(), sumo:schema_name()) -> [#hook{}].
hook_funs(Trigger, Module) ->
  lists:filtermap(fun(#hook{type = Type, func = Fun}) when Type =:= Trigger ->
                      {true, Fun};
                  (_Entry) ->
                      false
               end, Module:schema()).

%% @private
-spec run_hook_with_arg(
        on_create | on_delete | on_update,
        sumo:schema_name(),
        term()
      ) ->
  ok.
run_hook_with_arg(Trigger, Module, Arg) ->
  lists:foreach(fun(Fun) -> Fun(Arg) end, hook_funs(Trigger, Module)), ok.

%% @private
-spec run_hook_without_arg(
        on_schema_create | on_delete_all,
        sumo:schema_name()
      ) -> ok.
run_hook_without_arg(Trigger, Module) ->
  lists:foreach(fun(Fun) -> Fun() end, hook_funs(Trigger, Module)), ok.
