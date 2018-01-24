%%%-------------------------------------------------------------------
%% @doc router public API
%% @end
%%%-------------------------------------------------------------------

-module(router_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start_deps() ->
  application:start(sasl),	
  application:start(crypto),
  application:start(cowlib),
  application:start(ranch),
  application:start(cowboy),
  application:ensure_all_started(hackney),
  Options = [{timeout, 150000}, {max_connections, 200}],
  hackney_pool:start_pool(router_pool, Options).
  

start(_StartType, _StartArgs) ->
  start_deps(),
  ets:new(procs, [set, named_table, public]),
  Dispatch = cowboy_router:compile([
    {'_', [{"/store/:id", store_proxy, []}]} 
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{ 
    env => #{dispatch => Dispatch} }
  ),
  router_sup:start_link(). 

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
