Riak Ensemble Experiment
========================

Configured via riak_ensemble_demo.app.src:

 - data_root: path to the files where ensemble state saved (will append nodename)
 - nodes: list of nodes that will attempt to auto-join

The cluster will only form if the majority of the configured nodes are online and
running the application.

API
===

riak_ensemble_demo:read(Key) -> {ok, Value} | {error, not_found}
----------------------------------------------------------------
Returns value for key, or not_found.

riak_ensemble_demo:read_object(Key) -> {ok, Obj#obj} | {error, Error}
---------------------------------------------------------------------
Returns an tuple (actually an instance of the obj record from riak_ensemble_demo) of
{obj,{Epoch,Seq,Key,Value}} or an error.

Note that a not found is represented as #obj{value=notfound}.

riak_ensemble_demo:write(Key,Value) -> ok | {error, Error}
----------------------------------------------------------

Write value for key, over-writing any existing value.

riak_ensemble_demo:cas(Key, OldObj, NewValue) -> ok | {error, Error}
--------------------------------------------------------------------

Performs compare-and-swap. OldObj must be an instance of the record retrieved from a read_object operation.

riak_ensemble_demo:write_once(Key,Value) -> ok | {error, Error}
---------------------------------------------------------------

Write a value, only if the key is not already defined.

riak_ensemble_demo:cluster_status()
-----------------------------------

Print out the status of the cluster.

Testing
=======

To test, start up three shells (note the different node names):

```
erl -pa ebin -pa deps/*/ebin -name node1@127.0.0.1
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
(node1@127.0.0.1)1>  application:ensure_all_started(riak_ensemble_demo).
```

```
erl -pa ebin -pa deps/*/ebin -name node2@127.0.0.1
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
(node2@127.0.0.1)1>  application:ensure_all_started(riak_ensemble_demo).
```

```
erl -pa ebin -pa deps/*/ebin -name node3@127.0.0.1
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
(node3@127.0.0.1)1>  application:ensure_all_started(riak_ensemble_demo).
```

One of the nodes should eventuall print something like:
```
{ok,[syntax_tools,compiler,goldrush,lager,
     riak_ensemble_demo]}
15:00:00.128 [info] {root,'node1@127.0.0.1'}: Leading
15:00:00.487 [info] join(Vsn): {1,6} :: 'node2@127.0.0.1' :: ['node1@127.0.0.1']
15:00:04.145 [info] join(Vsn): {1,19} :: 'node3@127.0.0.1' :: ['node1@127.0.0.1','node2@127.0.0.1']
```

Which is telling you that the nodes have joined. As soon as a majority of the
nodes are online and joined, you can start issuing commands.

Implementation notes
====================

I wanted to build a cluster that would bootstrap itself based on a configuration
setting. The tricky part there is that riak_ensemble only wants to be enabled
on a single node in the cluster.

To accomplish this boostraping, freshly booted erlang nodes try to join the
erlang cluster, then try to find a peer that has the ensemble enabled. If none
can be found, it checks to ensure a quorum of the nodes that are configured are
reachable in the erlang cluster and that they are running the cluster manager
service. If these conditions are met, a global transaction is attempted, and
whichever node acquires the lock enabled the ensemble. Nodes that can find a
peer that has riak_ensemble enabled will attempt to join the cluster.

Also, I'm simply using the root ensemble at this point.