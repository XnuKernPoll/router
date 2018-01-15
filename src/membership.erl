-module(membership).
-export([add_store_node/2, rm_store_node/1]). 

add_store_node(Index, Node) ->
  {NP, Nodes} = ets:lookup(membership, "store_nodes"), 
  Ring = {NP, lists:append({Index, Node}, Nodes) },
  ets:insert(membership, {"store_nodes", Ring}),
  {ok, Ring}. 

not_id(Node, ID) ->  
  {Index, _} = Node,
  Index =/= ID. 

rm_store_node(Index) -> 
  {_, CHash} = ets:lookup(membership, "store_nodes"),
  {NP, Nodes} = CHash, 
  Ring = {NP, lists:filter(fun (X) -> not_id(X, Index) end, Nodes)},
  ets:insert(membership, {"store_nodes", Ring}),
  {ok, Ring}. 

