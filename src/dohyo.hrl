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

-type field_name() :: atom().
-type field_type()  ::
        integer | string | binary | text | float | date | datetime.
-type field_attr()  ::
        id | unique | index | not_null | auto_increment | {length, integer()}.
-type field_attrs() :: [field_attr()].
-record(field, { name       :: field_name(),
                 type       :: field_type(),
                 attrs = [] :: field_attrs() }).
-type field() :: #field{}.

-type validation_type() ::
        by | presence | inclusion | exclusion | format | length.
-type validation_args() :: [validation_arg()].
-type validation_arg() :: term().
-type field_validator_fun() :: fun((term()) -> boolean()).
-type doc_validator_fun() :: fun((sumo:user_doc()) -> boolean()).
-type validation_error() :: {true, {field_name(), validation_error_key()}}.
-type validation_error_key() :: bad_format | not_excluded | not_included |
                                not_present | bad_length.
-record(validation, { type  :: validation_type(),
                      field :: field_name(),
                      args  :: validation_args() }).
-type validation() :: #validation{}.
-type length_arg() :: pos_integer()| {non_neg_integer(), pos_integer()}.

-type association_type() :: belongs_to | has_many.
-type association_name() :: atom().
-type association_opts() :: #{ schema      => atom(),
                               polymorphic => boolean(),
                               as          => atom(),
                               local_key   => atom(),
                               foreign_key => atom(),
                               attrs       => field_attrs()
                             }.
-record(association, { type          :: association_type(),
                       name          :: association_name(),
                       options = #{} :: association_opts() }).
-type association() :: #association{}.

-type modifier_type() ::
        before_validate | after_validate | before_delete_by | before_delete |
        before_commit | after_read | after_read_many.
-type modifier_fun() :: fun((sumo:user_doc(), term()) -> sumo:user_doc()).
-record(modifier, {type :: modifier_type(), func :: modifier_fun()}).
-type modifier() :: #modifier{}.

-type hook_type() ::
        on_create | on_update | on_delete | on_delete_all | on_schema_create.
-type hook_fun() :: fun((sumo:user_doc(), term()) -> ok).
-record(hook, {hook, type :: hook_type(), func :: hook_fun()}).
-type hook() :: #hook{}.

-type schema() :: [schema_entry()].
-type schema_entry() ::
        field() | validation() | association() | modifier() | hook().
