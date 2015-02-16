-module(article).

-behaviour(dohyo_model).

-export([schema/0]).

-include_lib("dohyo/include/dohyo_model.hrl").

schema() ->
  [ % Fields
    dohyo:field(id, integer, [id, not_null, auto_increment]),
    dohyo:field(title, text, [not_null]),
    dohyo:field(content, text, [not_null]),

    % Associations
    dohyo:has_many(comments, [{schema, comment}]),
    dohyo:belongs_to(author, [{attrs, [not_null]}]),
    dohyo:has_many(commenters, [{schema, author},{through, comments}]),

    % Validations
    dohyo:validate(presence, author_id),
    dohyo:validate(presence, content),
    dohyo:validate(uniqueness, title, ?MODULE),

    % Modifiers
    dohyo:before_validate(fun assign_author/2),
    dohyo:after_validate(fun create_stub/2),
    dohyo:before_commit(fun create_cache_id/2),

    % Hooks
    dohyo:on_create(fun send_email/2),
    dohyo:on_update(fun send_email/2),
    dohyo:on_delete(fun send_email/2),
    dohyo:on_delete_all(fun send_email/2)
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
