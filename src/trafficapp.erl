-module(trafficapp).
-include("trafficapp.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
  ping/0,
  update/2,
  update/3,
  fetch/1,
  fetch/2]).

-ignore_xref([
  ping/0
]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, trafficapp),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, trafficapp_vnode_master).

update({deg, Zoom, TileLat, TileLon}, Payload) when Zoom == 14->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  update({num, Zoom, TileX, TileY}, Payload);

update({num, Zoom, TileX, TileY}, Payload) when Zoom == 14->
  update({num, Zoom, TileX, TileY}, {_OSMWayID = undefined, _OSMNodeID = undefined}, Payload).

update({deg, Zoom, TileLat, TileLon}, {OSMWayID, OSMNodeID}, Payload) when Zoom == 14->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  update({num, Zoom, TileX, TileY}, {OSMWayID, OSMNodeID}, Payload);

update({deg, Zoom, TileLat, TileLon}, OSMWayID, Payload) when Zoom == 14->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  update({num, Zoom, TileX, TileY}, OSMWayID, Payload);

update({num, Zoom, TileX, TileY}, {OSMWayID, OSMNodeID}, Payload) when Zoom ==14 ->
  begin
    N = 3,
    W = 3,
    Timeout = 5000, % millisecs

    TileHash = zoomTile2hash(num, Zoom, TileX, TileX),
    TileRec = #num{
      tile_dir_path = integer_to_list(Zoom) ++ "_" ++ integer_to_list(TileX) ++ "_" ++ integer_to_list(TileY),
      tile_sub_list = make_tile_sub_list(OSMWayID, OSMNodeID),
      tile_md5 = TileHash,
      tile_reference="reference",
      csum = erlang:adler32(Payload)
    },
    {ok, ReqId} = trafficapp_op_fsm:op(N, W, {update, TileRec, Payload}, ?KEY(TileHash))
  end,
  wait_for_reqid(ReqId, Timeout);

update({num, Zoom, TileX, TileY}, {OSMWayID}, Payload) when Zoom ==14 ->
  update({num, Zoom, TileX, TileY}, {OSMWayID, _OSMNodeID = undefined}, Payload).

fetch({deg, Zoom, TileLat, TileLon}) when Zoom == 14 ->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  fetch({num, Zoom, TileX, TileY}, {undefined, undefined});

fetch({num, Zoom, TileX, TileY}) when Zoom == 14 ->
  fetch({num, Zoom, TileX, TileY}, {_OSMWayID = undefined, _OSMNodeID = undefined}).



fetch({deg, Zoom, TileLat, TileLon}, {OSMWayID, OSMNodeID}) when Zoom == 14 ->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  fetch({num, Zoom, TileX, TileY}, {OSMWayID, OSMNodeID});

fetch({num, Zoom, TileX, TileY}, {OSMWayID, OSMNodeID}) when Zoom == 14 ->
  TileHash = zoomTile2hash(num, Zoom, TileX, TileX),
  TileRec = #num{
    tile_dir_path = integer_to_list(Zoom) ++ "_" ++ integer_to_list(TileX) ++ "_" ++ integer_to_list(TileY),
    tile_sub_list = make_tile_sub_list(OSMWayID, OSMNodeID),
    tile_md5 = TileHash,
    tile_reference="reference",
    csum = 0
  },
  Idx = riak_core_util:chash_key(?KEY(TileHash)),
  % TODO: Get a preflist with more than one node
  [{Node, _Type}] = riak_core_apl:get_primary_apl(Idx, 1, trafficapp),
  riak_core_vnode_master:sync_spawn_command(Node, {fetch, TileRec}, trafficapp_vnode_master);

fetch({num, Zoom, TileX, TileY}, OSMWayID) when Zoom == 14 ->
  fetch({num, Zoom, TileX, TileY}, {OSMWayID, undefined});

fetch({deg, Zoom, TileLat, TileLon}, {OSMWayID}) when Zoom == 14 ->
  {TileX, TileY} = deg2num(TileLat, TileLon, Zoom),
  fetch({num, Zoom, TileX, TileY}, {OSMWayID, undefined}).

make_tile_sub_list(OSMWayID, OSMNodeID)->
  case {OSMWayID, OSMNodeID} of
    {undefined, undefined} ->
      [];
    {OSMWayID, undefined} ->
      [integer_to_list(OSMWayID)];
    {OSMWayID, OSMNodeID} ->
      [integer_to_list(OSMWayID), integer_to_list(OSMNodeID)]
  end.

%%% convert an OSM node list to tiles
node2tile([], _Zoom) ->
  [];
node2tile([{Lat, Long} | Rest], Zoom) ->
  [deg2num(Lat, Long, Zoom) | node2tile(Rest, Zoom)].

deg2num(Lat, Lon, Zoom) ->
  X = math:pow(2, Zoom) * ((Lon + 180) / 360),
  Sec = 1 / math:cos(deg2rad(Lat)),
  R = math:log(math:tan(deg2rad(Lat)) + Sec) / math:pi(),
  Y = math:pow(2, Zoom) * (1 - R) / 2,
  {round(X), round(Y)}.

num2deg(X, Y, Zoom) ->
  N = math:pow(2, Zoom),
  Lon = X / N * 360 - 180,
  Lat_rad = math:atan(math:sinh(math:pi() * (1 - 2 * Y / N))),
  Lat = Lat_rad * 180 / math:pi(),
  {Lon, Lat}.


deg2rad(C) ->
  C * math:pi() / 180.

splitListAtTileBorders(OSMTileList, OSMTileUniqueOrdset, NodeList) ->
  splitListAtTileBorders(OSMTileList, OSMTileUniqueOrdset, NodeList, []).

splitListAtTileBorders(OSMTileList, [], _NodeList, Acc) ->
  lists:reverse(Acc);

splitListAtTileBorders(OSMTileList, [OSMTileUnique | Rest], NodeList, Acc) ->
  OSMTileAndNodeList = lists:zip(OSMTileList, NodeList),
  {NodeListResult, _Rest} = lists:partition(fun(A) -> {Tile, _} = A, Tile == OSMTileUnique end, OSMTileAndNodeList),
  splitListAtTileBorders(OSMTileList, Rest, NodeList, [NodeListResult | Acc]).

zoomTile2hash(num, Zoom, TileX, TileY) ->
  crypto:hash(md5, list_to_binary(integer_to_list(Zoom bsl 32 + (TileX bsl 16) + TileY))).

wait_for_reqid(Id, Timeout) ->
  receive {Id, Value} -> {ok, Value}
  after Timeout -> {error, timeout}
  end.
