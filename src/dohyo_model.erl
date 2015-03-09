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
sumo_field(#association{type = belongs_to, name = Name, options = Opts}) ->
  Field = dohyo_associations:local_key(belongs_to, undefined, Name, Opts),
  {true, sumo:new_field(Field, integer, field_attrs(Opts))};
sumo_field(_Entry) ->
  false.

%% @private
-spec field_attrs(association_opts()) -> [atom()].
field_attrs(#{attrs := Attrs}) ->
  Attrs;
field_attrs(_Opts) ->
  [].
