%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2015, Mark Steele
%%% @doc
%%% Watch cluster, auto-join configured nodes, bootstrap root ensemble
%%% @end
%%% Created : 19 Oct 2015 by Mark Steele <mark@control-alt-del.org>
%%%-------------------------------------------------------------------
-module(riak_ensemble_demo_cluster).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, NodeList} = application:get_env(riak_ensemble_demo, nodes),
  schedule_tick(),
  %% TODO: Link to riak_ensemble_sup?
  {ok, #state{nodes=NodeList}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(tick, State) ->
  State2 = tick(State),
  schedule_tick(),
  {noreply, State2};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
schedule_tick() ->
    erlang:send_after(1000, self(), tick).

tick(State=#state{nodes=Nodes}) ->
  maybe_bootstrap_ensembles(Nodes),
  State.

maybe_bootstrap_ensembles(Nodes) ->
  case riak_ensemble_manager:enabled() of
    false ->
      OnlineNodes = online_nodes(Nodes),
      QuorumNum = quorum_num(Nodes),
      EnabledNodes = find_enabled_node(OnlineNodes),

      if
        (EnabledNodes =:= []) and (length(OnlineNodes) >= QuorumNum) ->
          global:trans({ensemble_bootstrap,self()},
                       fun() ->
                           try
                             riak_ensemble_manager:enable(),
                             wait_stable(),
                             ok
                           catch
                             _:_ ->
                               error
                           end
                       end,
                       OnlineNodes,0);
        EnabledNodes =/= [] ->
          join_cluster(EnabledNodes);
        true ->
          ok
      end;
    true ->
      ok
  end.

quorum_num(Nodes) ->
  trunc(length(Nodes)/2)+1.

online_nodes(Nodes) ->
  NodeStatus = [
                begin
                  case net_adm:ping(X) of
                    pong ->
                      case catch rpc:call(X,erlang,whereis,[?MODULE]) of
                        Y when is_pid(Y) ->
                          {X, ok};
                        _ ->
                          {X, error}
                      end;
                    _ ->
                      {X, error}
                  end
                end || X <- Nodes],
  proplists:get_keys(lists:filter(fun({X,Y}) -> {X,Y} =:= {X,ok} end,NodeStatus)).

wait_stable() ->
  case check_stable() of
    true ->
      ok;
    false ->
      wait_stable()
  end.

check_stable() ->
  case riak_ensemble_manager:check_quorum(root, 1000) of
    true ->
      case riak_ensemble_peer:stable_views(root, 1000) of
        {ok, true} ->
          true;
        _ ->
          false
      end;
    false ->
      false
  end.

find_enabled_node(Nodes) ->
  lists:filter(
    fun(X) ->
        true =:= rpc:call(X,riak_ensemble_manager,enabled,[])
    end,
    Nodes
   ).

join_cluster([H|_T]) ->
  case riak_ensemble_manager:join(H,node()) of
    ok ->
      wait_stable(),
      riak_ensemble_peer:update_members(
        riak_ensemble_manager:get_leader_pid(root),
        [{add,{root,node()}}],
        5000),
      ok;
    _ ->
      join_cluster([H])
  end.
