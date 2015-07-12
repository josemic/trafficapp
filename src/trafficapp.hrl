-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(KEY(X), {<<"trafficapp">>, X}).

-record(num, {
        zoom,
        tile_x,
        tile_y,
        tile_md5,
        tile_dir_path,
        tile_sub_list,
        osmwayID,
        osmnodeID,
        tile_reference,
        csum,
        version
}).
