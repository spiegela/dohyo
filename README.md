# Dohyo Model
Dohyo is a relational mapper inspired by Rails ActiveRecord, while striving to
retain functional programming characteristics.

Dohyo adds features for data modeling including:
* Field Validations
* Relational Associations
* Synchronous Entity Modifiers
* Asynchronous Event Hooks

It leans heavily on [SumoDB](https://github.com/inaka/sumo_db) for the base
schema definition and database connectivity.

## Schema Definition Functions

### Field definition

* `dohyo:field/2`
* `dohyo:field/3`

```erlang
dohyo:field(id, integer, [id, not_null, auto_increment])

dohyo:field(title, text)
```

These functions mimic the functionality of
[SumoDB](https://github.com/inaka/sumo_db). Aliases are provided to keep the
schema definition looking clean.

### Association definition

* `has_many/1`
* `has_many/2`
* `belongs_to/1`
* `belongs_to/2`

```erlang
dohyo:has_many(comments, [{schema, comment}])

dohyo:belongs_to(author)
```

These functions create one-to-many or many-to-one relationships.  Options can
customize the behaviour of queries that are executed to fetch relationships.

Local keys (in the case of `belongs_to` relationships need not be create with
`field` calls, as dohyo will do that for you.


### Field validation definition

* `validate/2`
* `validate/3`

These functions generate schema validation entris with the following valid
types:
* presence
* inclusion
* exclusion
* format
* length

```erlang
%% Check presence of fields
dohyo:validate(presence, author)

%% Check that they're included in some list of good values
dohyo:validate(inclusion, some_field, ["value1", "value2", "value3"])

%% or that they're excluded in some list of bad values
dohyo:validate(exclusion, some_field, ["badvalue1", "badvalue2"])

%% Check them against a `re` compatible regex string.  (don't use that pattern
%% for actual email checking)
dohyo:validate(format, email, "\\w+@\\w+\[.\\w+]+")

%% or that values are long enough
dohyo:validate(length, email, 5)
```

Important validations left add or improve:
* length -- need to add maximum
* uniqueness
* by (use a generic validation function)

### Entity modifier definition

* `before_validate`
* `after_validate`
* `before_commit`
* `before_delete`
* `before_delete_by`
* `after_read`

These functions take a function with the follow spec:

```erlang
Fun(Entity :: proplists:proplist(), State :: term()) ->
  Entity2 :: proplists:proplist().

dohyo:before_validate(fun(Entity, State) ->
                        RandoTup = lists:keyfind(random, 1, State),
                        lists:keystore(random, 1, Entity, RandoTup)
                      end).
```

This allows you to change the values in an entity before a persistence change,
while allowing you to integrate the current state of your application.

You can specify multiple modifiers for a single hook, so you can keep these
functions small and focused.

### Entity hooks definition

* `on_create/1`
* `on_update/1`
* `on_delete/1`
* `on_delete_all/1`
* `on_schema_create/1`

These specific hook handlers that will listen on automatically started event
handlers subscribed to the SumoDB event bus.  They occur asynchrously, and
only have access to the data that the event handler produces.  Your hook
functions should expect the appropriate arguments for each event:

```erlang
dohyo:on_create(fun(Entity) -> ... end)

dohyo:on_update(fun(Entity) -> ... end)

dohyo:on_delete(fun(Id) -> ... end)

dohyo:on_delete_all(fun() -> ... end)

dohyo:on_schema_create(fun() -> ... end)
```

Keep in mind that these are created within the context of a schema, and these
functions will only apply to the schema they're defined in -- not globally.

## Examples

Dohyo is used in two ways within you application:
1. As a way to define you model schema
2. An interface module to simplify common data-model tasks

### Defining your schema

```erlang
-module(article).

-behaviour(dohyo_model).

%% dohyo_model behaviour callback
-export([schema/0]).

%% Import functions to define the sumo_db behaviour callbacks (included in the
%% dohyo_model behaviour as well)
-include_lib("dohyo/include/dohyo_model.hrl").

schema() ->
  [ % Define model fields
    dohyo:field(id, integer, [id, not_null, auto_increment]),
    dohyo:field(title, text, [not_null]),
    dohyo:field(content, text, [not_null]),

    % Define model associations -- note, no need to specify the "author_id."
    % field, as this is done by dohyo
    dohyo:has_many(comments, [{schema, comment}]),
    dohyo:belongs_to(author, [{attrs, [not_null]}]),

    % Define some field validations to keep the model clean. Attempting to
    % Persist an entity with failures here will precipitate a validation
    % error.
    dohyo:validate(presence, author_id),
    dohyo:validate(presence, content),
    dohyo:validate(uniqueness, title, ?MODULE),

    % Define modifications that take place before a model is persisted or after
    % it is read.  These can be used to integrate application state
    % (http-session vars, time, etc.) into the written entity, or can be used
    % for generated field data like encrypted password, stubs, etc.
    % The important characteristic is that it modifies the entity in line.
    dohyo:before_validate(fun assign_author/2),
    dohyo:after_validate(fun create_stub/2),
    dohyo:before_commit(fun create_cache_id/2),

    % Define asynchronous actions to occur with database activities.  These
    % actions do not modify the record, rather they fire through an
    % event_handler.
    dohyo:on_create(fun send_email/2),
    dohyo:on_update(fun send_email/2),
    dohyo:on_delete(fun send_email/2),
    dohyo:on_delete_all(fun send_email/1)
  ].


send_email(_Entity, _State) ->
  %% More code here...
  ok.

assign_author(Entity, _State) ->
  %% More code here...
  Entity.

create_stub(Entity, _State) ->
  %% More code here...
  Entity.

create_cache_id(Entity, _State) ->
  %% More code here...
  Entity.
```

Dohyo offers a way to cleanly organize functions as a cohesive model, while
keeping things functional, and keeping things (pretty much) transparent.

To interact with your model, you'll use the interface API:

```erlang
%%% CRUD operations

1> dohyo:persist(article, [{title, "test2"}, {content, "Another Post"}].

[{id, 123}, {title, "test"}, {content, "F1st Post"}, {author_id, 3},
 {stub, "2015-01-01-test"}, {cache_id, 123}
]

%% Validations are run prior to persistence, modifiers run for auto-generated
%% fields, and then the doc is written to the DB.

2> dohyo:find_one(article, [{id,123}], State).

[{id, 123}, {title, "test"}, {content, "F1st Post"}, {author_id, 3},
 {stub, "2015-01-01-test"}, {cache_id, 123}
]

%%% Associations

3> dohyo:associate(article, author, Entity)

%% Execute a select to get the author details, and insert the author-entity
%% into the article property list.

4> dohyo:associate_all(article, Entity)

%% Execute multiple selects to get all of the article associations, and insert
%% them into the article property list.

5> dohyo:association(article, author, Entity)

%% Execute a select to get the author details, and return a tuple-pair with the
%% association name (author) and the author property list.

6> dohyo:all_associations(article, Entity)

%% Execute multiple selects to get all of the article associations, return a
%% new property list of associations.

```


### TODOs

TODOs are on the [github issue log](http://github.com/spiegela/dohyo/issues).

## License

The MIT License (MIT)

Copyright (c) 2015 Aaron spiegel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

