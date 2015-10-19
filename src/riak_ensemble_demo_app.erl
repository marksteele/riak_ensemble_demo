-module(riak_ensemble_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = riak_ensemble_demo_sup:start_link(),
  riak_ensemble_manager:enable(),
  wait_stable(root),
%% Autojoin cluster together?
%  {ok, NodeList} = application:get_env(riak_ensemble_demo, nodes),
%  TargetNodes = [ {root,list_to_atom(X)} || X <- string:tokens(NodeList,",") ],
%  Members = riak_ensemble_manager:get_members(root),
  Ret.

stop(_State) ->
  ok.

wait_stable(Ensemble) ->
  case check_stable(Ensemble) of
    true ->
      ok;
    false ->
      wait_stable(Ensemble)
  end.

check_stable(Ensemble) ->
  case riak_ensemble_manager:check_quorum(Ensemble, 1000) of
    true ->
      case riak_ensemble_peer:stable_views(Ensemble, 1000) of
        {ok, true} ->
          true;
        _Other ->
          false
      end;
    false ->
      false
  end.
