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

-module(dohyo_validations).

%%%
%%% Validators
%%%
-export([presence/2, inclusion/3, exclusion/3, format/3, length/3, by/3]).
% -export([numericality/2, uniqueness/2, with/2]).

%%%
%%% Validation Actions
%%%
-export([validate/2]).

-include_lib("dohyo.hrl").

%% Public API

%% @doc
%% Tests proplist to see if it contains a field.  Return type is compatible
%% with lists:filtermap/2.
%% @end
-spec presence(proplists:proplist(), field_name()) ->
  false | {true, {field_name(), not_present}}.
presence(Plist, Field) ->
  presence2(Field, lists:keyfind(Field, 1, Plist)).

%% @doc
%% Tests proplist to see if field's value is included within the inclusion
%% list. Return type is compatible with lists:filtermap/2.
%% @end
-spec inclusion(proplists:proplist(), field_name(), [term()]) ->
  false | {true, {field_name(), not_included}}.
inclusion(Plist, Field, Values) ->
  inclusion2(Field, lists:keyfind(Field, 1, Plist), Values).

%% @doc
%% Tests proplist to see if field's value is excluded from the exclusion
%% list. Return type is compatible with lists:filtermap/2.
%% @end
-spec exclusion(proplists:proplist(), field_name(), [term()]) ->
  false | {true, {field_name(), not_excluded}}.
exclusion(Plist, Field, Values) ->
  exclusion2(Field, lists:keyfind(Field, 1, Plist), Values).

%% @doc
%% Tests proplist to see if field's value matches the regular expression
%% string. Return type is compatible with lists:filtermap/2.
%% @end
-spec format(proplists:proplist(), field_name(), string()) ->
  false | {true, {field_name(), bad_format}}.
format(Plist, Field, Format) ->
  format2(Field, lists:keyfind(Field, 1, Plist), Format).

%% @doc
%% Tests proplist to see if field's value matches the provided length. Return
%% type is compatible with lists:filtermap/2.
%% @end
-spec length(proplists:proplist(), field_name(), length_arg()) ->
  false | {true, {field_name(), bad_length}}.
length(Plist, Field, Length) ->
  length2(Field, lists:keyfind(Field, 1, Plist), Length).

%% @doc
%% Tests proplist field value against provided value. Return type is compatible
%% with lists:filtermap/2.
%% @end
-spec by(proplists:proplist(), field_name(), field_validator_fun()) ->
  false | {true, {field_name(), bad_length}}.
by(Plist, Field, Fun) -> by2(Field, lists:keyfind(Field, 1, Plist), Fun).

%%% Private Functions

%% @private
-spec presence2(field_name(), false | {field_name(), term()}) ->
  false | {true, {field_name(), not_present}}.
presence2(Field, false)   -> {true, {Field, not_present}};
presence2(_Field, _Tuple) -> false.

%% @private
-spec inclusion2(field_name(), false | {field_name(), term()}, [term()]) ->
  false | {true, {field_name(), not_included}}.
inclusion2(_Field, false, _Values) ->
  false;
inclusion2(Field, {Field, Value}, Values) ->
  inclusion3(Field, lists:member(Value, Values)).

%% @private
-spec exclusion2(field_name(), false | {field_name(), term()}, [term()]) ->
  false | {true, {field_name(), not_excluded}}.
exclusion2(_Field, false, _Values) ->
  false;
exclusion2(Field, {Field, Value}, Values) ->
  exclusion3(Field, lists:member(Value, Values)).

%% @private
-spec inclusion3(field_name(), boolean()) ->
  false | {true, {field_name(), not_included}}.
inclusion3(_Field, true) -> false;
inclusion3(Field, false) -> {true, {Field, not_included}}.

%% @private
-spec exclusion3(field_name(), boolean()) ->
  false | {true, {field_name(), not_excluded}}.
exclusion3(_Field, false) -> false;
exclusion3(Field, true) -> {true, {Field, not_excluded}}.

%% @private
-spec format2(field_name(), false | {field_name(), term()}, string()) ->
  false | {true, {field_name(), bad_format}}.
format2(_Field, false, _Format) ->
  false;
format2(Field, {Field, Value}, Format) ->
  format3(Field, re:run(Value, Format)).

%% @private
-spec format3(field_name(), nomatch | {match, {integer(), integer()}}) ->
  false | {true, {field_name(), bad_format}}.
format3(Field, nomatch) -> {true, {Field, bad_format}};
format3(_Field, {match, _Match}) -> false.

%% @private
-spec length2(field_name(), false | {field_name(), term()}, length_arg()) ->
  false | {true, {field_name(), bad_length}}.
length2(_Field, false, _Length) ->
  false;
length2(Field, {Field, Value}, {Min, Max}) ->
  Length = length(Value),
  length3(Field, Length >= Min andalso Length =< Max);
length2(Field, {Field, Value}, Length) ->
  length3(Field, length(Value) =:= Length).

%% @private
-spec length3(field_name(), boolean()) ->
  false | {true, {field_name(), bad_length}}.
length3(_Field, true) -> false;
length3(Field, false) -> {true, {Field, bad_length}}.

%% @private
-spec by2( field_name(),
           false | {field_name(), term()}, field_validator_fun()
         ) ->
  false | {true, {field_name(), bad_length}}.
by2(_Field, false, _Fun) ->
  false;
by2(Field, {Field, Value}, Fun) ->
  case Fun(Value) of
    false -> {true, {Field, bad_validate_by}};
    true -> false
  end.

%% @doc
%% Run a dohyo schema's validations against the provided property-list.  Errors
%% will be first aggregated and then thrown if they exist.
%% @throws {invalid_doc, [validation_error()]}
%% @end
-spec validate(sumo:schema_name(), proplists:proplist()) -> ok.

validate(Module, Plist) ->
  Fun = fun(Validation) -> run_validation(Plist, Validation) end,
  case lists:filtermap(Fun, validations(Module:schema())) of
    [] -> ok;
    Errors -> throw({invalid_doc, Errors})
  end.

%% @private
-spec run_validation(proplists:proplist(), validation()) ->
  false | validation_error().
run_validation(_Plist, #validation{type = inclusion, args = undefined}) ->
  error(badarg);
run_validation(_Plist, #validation{type = exclusion, args = undefined}) ->
  error(badarg);
run_validation(_Plist, #validation{type = format, args = undefined}) ->
  error(badarg);
run_validation(_Plist, #validation{type = length, args = undefined}) ->
  error(badarg);
run_validation(_Plist, #validation{type = by, args = undefined}) ->
  error(badarg);
run_validation(Plist, #validation{type = presence, field = Field}) ->
  dohyo_validations:presence(Plist, Field);
run_validation(Plist, #validation{type = inclusion, field = Field,
                                  args = Values}) ->
  dohyo_validations:inclusion(Plist, Field, Values);
run_validation(Plist, #validation{type = exclusion, field = Field,
                                  args = Values}) ->
  dohyo_validations:exclusion(Plist, Field, Values);
run_validation(Plist, #validation{type = format, field = Field,
                                  args = Format}) ->
  dohyo_validations:format(Plist, Field, Format);
run_validation(Plist, #validation{type = length, field = Field,
                                  args = Length}) ->
  dohyo_validations:length(Plist, Field, Length);
run_validation(Plist, #validation{type = by, field = undefined,
                                  args = Fun}) ->
  dohyo_validations:by(Plist, Fun);
run_validation(Plist, #validation{type = by, field = Field,
                                  args = Fun}) ->
  dohyo_validations:by(Plist, Field, Fun).

%% @private
-spec validations(schema()) -> [validation()].
validations(Schema) -> lists:filter(fun is_validation/1, Schema).

%% @private
-spec is_validation(schema_entry()) -> boolean().
is_validation(#validation{}) -> true;
is_validation(_Entry) -> false.
