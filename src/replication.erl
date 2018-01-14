-module(replication).
-export([replication_proc/0, get_replica_list/2]).

term_fun(Node) -> 
  {_, T} = Node,
  T. 

gen_uri(Host, Req) -> 
  Base = list_to_binary(Host), 
  hackney_url:make_url(Base, cowboy_req:path(Req), cowboy_req:qs(Req)).  
  
get_replica_list(Ring, ID) -> 
  F = application:get_env(replica_factor),  
  Replicas0 = chash:successors(ID, Ring, F), 
  lists:map(fun (Entry) -> term_fun(Entry) end, Replicas0).

send_part_bin(Ref, Body) ->
  hackney:send_multipart_body(Ref, {part_bin, Body}). 

last_part(Ref, Body) ->
  hackney:send_multipart_body(Ref, {part_bin, Body}), 
  hackney:send_multipart_body(Ref, {part, eof}),    
  hackney:send_multipart_body(Ref, eof).


% maybe I should make the foreach over the Ref lists parallel with spawn in stream_to_replicas% 
stream_to_replicas(Req0, Refs) ->
  case cowboy_req:read_part_body(Req0) of 
    {ok, _LastBodyChunk, Req} ->
      lists:foreach(fun (Ref) -> last_part(Ref, _LastBodyChunk) end, Refs), 
      ok;
    {more, _Body, Req} ->
      lists:foreach(fun (Ref) -> send_part_bin(Ref, _Body) end, Refs),
      stream_to_replicas(Req, Refs)
  end.


init_upload(URL, ID) -> 
  {ok, Ref} = hackney:request(post, URL, [], stream_multipart, []),
  hackney:send_multipart_body(Ref, {part, ID}),
  Ref. 


replication_proc() -> 
  receive 
    {replicate, Replicas, Req} -> 
      Refs = lists:map(fun (Host) -> init_upload(gen_uri(Host, Req), Replicas) end,  Replicas), 
      stream_to_replicas(Req, Refs),
      replication_proc() 
  end.
  
