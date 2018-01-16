%% @doc Member Management module, remember that the add and remove functions operate on a public ets table called membership. 

-module(membership).
-export([add_store_node/2, rm_store_node/1, add_router_node/1, rm_router_node/1]). 

-type host_port() :: {string(), integer()}.  
-type peer() :: {string(), integer(), binary()}. 

-export_type([host_port/0, peer/0]).

-spec add_store_node(binary(), term()) -> {ok, chash:chash()}.
add_store_node(Index, Node) ->
  {NP, Nodes} = ets:lookup(membership, "store_nodes"), 
  Ring = {NP, lists:append(Nodes, [{Index, Node}]) },
  ets:insert(membership, {"store_nodes", Ring}),
  {ok, Ring}. 

-spec not_id(chash:node_entry(), binary()) -> boolean().  
not_id(Node, ID) ->  
  {Index, _} = Node,
  Index =/= ID. 

-spec not_peer(peer(), binary()) -> boolean().  
not_peer(Peer, ID) ->
  {_, _, PeerID} =  Peer,
  PeerID =/= ID. 

-spec rm_store_node(binary()) -> {ok, chash:chash()}.
rm_store_node(Index) -> 
  {_, CHash} = ets:lookup(membership, "store_nodes"),
  {NP, Nodes} = CHash, 
  Ring = {NP, lists:filter(fun (X) -> not_id(X, Index) end, Nodes)},
  ets:insert(membership, {"store_nodes", Ring}),
  {ok, Ring}. 

-spec add_router_node(peer()) -> {ok, [peer()]}. 
add_router_node(Peer) -> 
  {_, Peers} = ets:lookup(membership, "router_nodes"), 
  Peers0 = lists:append(Peers, [Peer]),
  ets:insert(membership, {"router_nodes", Peers0}),
  {ok, Peers0}. 

-spec rm_router_node(binary()) -> {ok, [peer()]}.  
rm_router_node(ID) ->
  {_, Peers} = ets:lookup(membership, "router_nodes"),
  Peers0 = lists:filter(fun (X) -> not_peer(X, ID) end, Peers),
  ets:insert(membership, {"router_nodes", Peers0}),
  {ok, Peers0}. 


