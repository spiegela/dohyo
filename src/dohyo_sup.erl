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

-module(dohyo_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0, start_hook_handler/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%% API functions

%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc
%% Starts a new hook handler
%%
%% @spec start_hook_handler() -> {ok, Pid} | ignore | {error, Error}
%% @end
start_hook_handler(Schema) ->
  Child = list_to_atom(lists:concat([atom_to_list(Schema),"_hook_handler"])),
  supervisor:start_child( ?MODULE,
                          ?CHILD(Child, dohyo_hook_handler, worker, [Schema])
                        ).


%%% Supervisor callbacks

%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%%% Internal functions
