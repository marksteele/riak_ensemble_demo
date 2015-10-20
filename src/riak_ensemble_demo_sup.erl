%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2015, Mark Steele
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2015 by root <>
%%%-------------------------------------------------------------------
-module(riak_ensemble_demo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
init([]) ->
  {ok, DataRoot} = application:get_env(riak_ensemble_demo,data_root),

  Ensemble = {riak_ensemble_sup, {riak_ensemble_sup, start_link, [DataRoot ++ atom_to_list(node())]},
            permanent, 20000, supervisor, [riak_ensemble_sup]},

  ClusterWatcher = {riak_ensemble_demo_cluster, {riak_ensemble_demo_cluster, start_link, []},
            permanent, 20000, worker, [riak_ensemble_demo_cluster]},

  {ok, {{one_for_one, 10, 30}, [Ensemble,ClusterWatcher]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
