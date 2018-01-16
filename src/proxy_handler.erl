-module(proxy_handler). 

stream_download(Ref, Rep) ->
  case hackney:stream_body(Ref) of 
    {ok, Data} ->
      cowboy_req:stream_body(Data, nofin, Rep),
      stream_download(Ref, Rep);
    done ->
      cowboy_req:stream_body(<<"">>, fin, Rep),
      ok
  end. 

uploader(ID, Req) ->
  {_, PID} = ets:lookup(procs, "replication"),
  {_, Chash} = ets:lokup(membership, "store_nodes"),  
  Replicas = replication:get_replica_list(Chash, ID),
  MSG = {replicate, Replicas, Req},
  PID ! MSG. 

deleter(ID, Req) ->
  {_, PID} = ets:lookup(procs, "replication"),
  {_, Chash} = ets:lokup(membership, "store_nodes"),  
  Replicas = replication:get_replica_list(Chash, ID),
  MSG = {delete, Replicas, Req},
  PID ! MSG. 

