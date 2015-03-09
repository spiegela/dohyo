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
    dohyo:has_many(comments, #{schema => comment}),
    dohyo:belongs_to(author, #{attrs =>[not_null]}),
    dohyo:has_many(commenters, #{schema => author, through => comments}),

    % Validations
    dohyo:validate(author_id, presence),
    dohyo:validate(content, presence),
    dohyo:validate(title, uniqueness, ?MODULE),

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
