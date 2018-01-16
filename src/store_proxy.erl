-module(store_proxy). 
-export([init/2]).

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
      cowboy_req:stream_body(<<"">>, fin, Rep)
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

download_from_replica(ID, Req, Rep) -> 
  {_, Chash} = ets:lookup(membership, "store_nodes"),
  Replicas = replication:get_replica_list(Chash, ID),
  Host = select_random(Replicas), 
  URI = replication:gen_uri(list_to_binary(Host), Req),
  Ref = hackney:get(URI, [], []),
  stream_download(Ref, Rep). 

upload_cb(ID, Req) -> 
  upload_to_replicas(ID, Req),
  Body = io_lib:format("File ~p was upload to store ~n", [ID]) ,
  cowboy_req:reply(200, #{}, Body, Req). 

delete_cb(ID, Req) -> 
  delete_on_replicas(ID, Req),
  Body = io_lib:format("File ~p was deleted from store ~n", [ID]) ,
  cowboy_req:reply(200, #{}, Body, Req). 
  
init(Req, State) -> 
  Method = cowboy_req:method(Req),
  ID0 = cowboy_req:binding(id, Req), 
  ID1 = [list_to_integer(Hex, 16) || Hex <- ID0], 
  ID = list_to_binary(ID1), 
  case Method of 
    <<"POST">> ->
      Rep = upload_cb(ID, Req),
      {ok, Rep, State};
    <<"GET">> -> 
      Rep1 = cowboy_req:stream_reply(200, Req),
      Rep = download_from_replica(ID, Req, Rep1),
      {ok, Rep, State};
    <<"DELETE">> ->
      Rep = delete_cb(ID, Req),
      {ok, Rep, State}
  end. 

