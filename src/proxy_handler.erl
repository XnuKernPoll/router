-module(proxy_handler). 

select_random(L) ->
  Size = length(L),
  I = random:uniform(Size),
  lists:nth(I, L).  

stream_download(Ref, Rep) ->
  case hackney:stream_body(Ref) of 
    {ok, Data} ->
      cowboy_req:stream_body(Data, nofin, Rep),
      stream_download(Ref, Rep);
    done ->
      cowboy_req:stream_body(<<"">>, fin, Rep),
      ok
  end. 

upload_to_replicas(ID, Req) ->
  {_, PID} = ets:lookup(procs, "replication"),
  {_, Chash} = ets:lookup(membership, "store_nodes"),  
  Replicas = replication:get_replica_list(Chash, ID),
  MSG = {replicate, Replicas, Req},
  PID ! MSG. 

delete_on_replicas(ID, Req) ->
  {_, PID} = ets:lookup(procs, "replication"),
  {_, Chash} = ets:lookup(membership, "store_nodes"),  
  Replicas = replication:get_replica_list(Chash, ID),
  MSG = {delete, Replicas, Req},
  PID ! MSG. 

handle_download(ID, Req) -> 
  {_, Chash} = ets:lookup(membership, "store_nodes"),
  Replicas = replication:get_replica_list(Chash, ID),
  Host = select_random(Replicas), 
  URI = replication:gen_uri(list_to_binary(Host), Req),
  Ref = hackney:get(URI, [], []),
  stream_download(Ref, Rep). 

