-module(riak_ensemble_demo).

-export([
         join/1,
         leave/0,
         cluster_status/0,
         read/1,
         read_object/1,
         write/2,
         cas/3,
         write_once/2
        ]).

-record(obj,{epoch,seq,key,value}).

read(Key) ->
  case read_object(Key) of
    {ok,#obj{value=notfound}} ->
      {error, not_found};
    {ok,Obj} ->
      {ok, Obj#obj.value};
    Error ->
      Error
  end.

read_object(Key) ->
  case riak_ensemble_client:kget(node(),root,Key,10000) of
    {ok,Obj} ->
      {ok, Obj};
    Error ->
      {error, Error}
  end.

write(Key, Value) ->
  case riak_ensemble_client:kover(node(),root,Key,Value,10000) of
    {ok, _} ->
      ok;
    Err ->
      {error, Err}
  end.

cas(Key, OldObj, New) ->
  case riak_ensemble_client:kupdate(node(),root,Key,OldObj,New,10000) of
    {ok,_} ->
      ok;
    Error ->
      {error, Error}
  end.

write_once(Key,Value) ->
  case riak_ensemble_client:kput_once(node(),root,Key,Value,10000) of
    {ok, _} ->
      ok;
    Error ->
      {error, Error}
  end.

join(Node) ->
  case riak_ensemble_manager:join(Node, node()) of
    ok ->
      ok;
    remote_not_enabled ->
      {error, "Ensemble not enabled on node"};
    Error ->
      {error, Error}
  end.

leave() ->
  RootLeader = riak_ensemble_manager:rleader_pid(),
  case riak_ensemble_peer:update_members(RootLeader, [{del, {root, node()}}], 10000) of
    ok ->
      case wait_for_root_leave(30) of
        ok ->
          NewRootLeader = riak_ensemble_manager:rleader_pid(),
          case riak_ensemble_manager:remove(node(NewRootLeader), node()) of
            ok ->
              ok;
            Error ->
              Error
          end;
        Error ->
          Error
      end;
    Error ->
      Error
  end.

wait_for_root_leave(Timeout) ->
  wait_for_root_leave(0, Timeout).

wait_for_root_leave(RetryCount, RetryLimit) when RetryCount =:= RetryLimit ->
  {error, timeout_waiting_to_leave_root_ensemble};
wait_for_root_leave(RetryCount, RetryLimit) ->
  case in_root_ensemble(node()) of
    true ->
      timer:sleep(1000),
      wait_for_root_leave(RetryCount + 1, RetryLimit);
    false ->
      ok
  end.

in_root_ensemble(Node) ->
  RootNodes = [N || {root, N} <- riak_ensemble_manager:get_members(root)],
  lists:member(Node, RootNodes).

cluster_status() ->
  case riak_ensemble_manager:enabled() of
    false ->
      {error, not_enabled};
    true ->
      Nodes = lists:sort(riak_ensemble_manager:cluster()),
      io:format("Nodes in cluster: ~p~n",[Nodes]),
      LeaderNode = node(riak_ensemble_manager:get_leader_pid(root)),
      io:format("Leader: ~p~n",[LeaderNode])
    end.
