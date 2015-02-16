-type field_name() :: atom().
-type field_type()  ::
        integer | string | binary | text | float | date | datetime.
-type field_attr()  ::
        id | unique | index | not_null | auto_increment | {length, integer()}.
-type field_attrs() :: [field_attr()].
-record(field, { name       :: field_name(),
                 type       :: field_type(),
                 attrs = [] :: field_attrs() }).

-type validation_type() ::
        presence | inclusion | exclusion | format | length.
-type validation_args() :: [validation_arg()].
-type validation_arg() :: term().
-type validation_error() :: {true, {field_name(), validation_error_key()}}.
-type validation_error_key() :: bad_format | not_excluded | not_included |
                                not_present | bad_length.
-record(validation, { type  :: validation_type(),
                      field :: field_name(),
                      args  :: validation_args() }).

-type association_type() :: belongs_to | has_many.
-type association_name() :: atom().
-record(association, { type         :: association_type(),
                       schema       :: module(),
                       name         :: association_name(),
                       options = [] :: proplists:proplist() }).

-type modifier_type() ::
        before_validate | after_validate | before_delete | before_commit |
        after_read.
-type modifier_fun() :: fun((sumo:user_doc(), term()) -> sumo:user_doc()).
-record(modifier, {type :: modifier_type(), func :: modifier_fun()}).

-type hook_type() ::
        on_create | on_update | on_delete | on_delete_all | on_schema_create.
-type hook_fun() :: fun((sumo:user_doc(), term()) -> ok).
-record(hook, {hook, type :: hook_type(), func :: hook_fun()}).

-type schema() :: [schema_entry()].
-type schema_entry() ::
        #field{} | #validation{} | #association{} | #modifier{} | #hook{}.

-type key_type() :: local_key | foreign_key.
