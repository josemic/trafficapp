trafficapp: A Riak Core based Application
======================================

Status: Proof of Concept

Purpose of the application
--------------------------

To store traffic data in a distributed database in riak core nodes, which may 
be stored on multiple machines.
. 
For more info see Github-Wiki.

The data is stored in the file system.

Concept:

Like Openstreetmap the data is stored, while only one layer. Currently the 
layer is fixed to 14. Within the layer the data is stored in tiles which can 
be either be addressed using
* 1. Degrees: lattitude and longitude or
* 2. Numbers: tile numbers X and tile number Y

Within the tile the traffic data shall be attached to way and one node of the 
way. (The format of the traffic data is currently not defined, while it might be 
wise to use Json, as it can be embedded into xml. Tbd.).

When retrieving the data from the database, either the data can be retrieved:
* 1. By addressing the tile
* 2. By addressing the tile and a specific way identified by its id
* 3. By addressing the tile, a specific way and a specifc node by their ids

In case of 2 (all data of the way) and 3 (all data of the tile) is accumulated. 
Note: 
It is assumed, that the OSM way and OSM nodes are loacated within the tile. 


A rest interface is provided.

Application Structure
---------------------

This application is based on riak core and rebar_riak_core: 


* https://github.com/basho/riak_core
* https://github.com/basho/rebar_riak_core

```
make rel
./rel/trafficapp/bin/trafficapp console
```

At this point you have a single node of trafficapp running. Lets test it via the erlang console:

```
1> trafficapp:ping().
{pong,753586781748746817198774991869333432010090217472}
```

Now it is time to store data:

Format:
```
trafficapp:update({deg, Level, Lat, Lon}, {OSMWayID, OSMNodeID}, Data).
or 
trafficapp:update({num, Level, TileX, TileY}, {OSMWayID, OSMNodeID}, Data).
```

```
2>trafficapp:update({deg, 14, 51, 52}, {123123123, 234234234}, "Data1").
3>trafficapp:update({deg, 14, 51, 52}, {123123123, 234235435}, "Data2").
```
Data could e.g. inform that there is a traffic jam and the highest speed is 5 mph.
Format tdb.

Now it is time to read data:

Format:
```
trafficapp:fetch({deg, Level, Lat, Lon}, {OSMWayID, OSMNodeID}).
trafficapp:fetch({deg, Level, Lat, Lon}, {OSMWayID}).
trafficapp:fetch({deg, Level, Lat, Lon}).
trafficapp:fetch({num, Level, TileX, TileY}, {OSMWayID, OSMNodeID}).
trafficapp:fetch({num, Level, TileX, TileY}, {OSMWayID}).
or
trafficapp:fetch({num, Level, TileX, TileY}).
```

Example:
```
4>trafficapp:fetch({deg, 14, 51, 52}, {123123123}).
...
[{ok,<<"Data1">>},{ok,<<"Data2">>}]
...
```

to fetch data associated to way 123123123 within tile {deg, 14, 51, 52}.

Now shut it down:

```
5> q().
```
.. and lets test it using the cmdline, and test it via http request:
```
./rel/trafficapp/bin/trafficapp start

./rel/trafficapp/bin/trafficapp ping
curl http://localhost:10018/trafficapp/ping
```

Example file update.xml:
```
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE osm_traffic SYSTEM "update.dtd">

<osm_traffic version="0.1" generator="manually">
  <traffic_api>
    <version minimum="0.1" maximum="0.1"/>
    <tile_deg level="14" lat="51" lon= "52"/>
    <waynodes maximum="2000"/>
    <wayref id="123123123"/>  <!-- optional, but mandatory if noderef below is given -->
    <noderef id="5000"/>  <!-- optional -->
    <payload> this is the payload </payload>
  </traffic_api>
</osm_traffic>
```

```
curl -v "http://localhost:10018/update" -d @deps/traffic_rest_app/xml/update.xml
curl -v "http://localhost:10018/update" -d @deps/traffic_rest_app/xml/update1.xml
curl -v "http://localhost:10018/update" -d @deps/traffic_rest_app/xml/update2.xml
curl -v "http://localhost:10018/update" -d @deps/traffic_rest_app/xml/update3.xml

curl -H "Content-Type:text/plain" \
"http://127.0.0.1:10018/fetch/deg?zoom=14&lat=51&lon=52&wayid=123123123&nodeid=5000"
>> [{ok,<<" this is the payload for way 123123123 node id 5000">>}]

curl -H "Content-Type:text/plain" \
"http://127.0.0.1:10018/fetch/deg?zoom=14&lat=51&lon=52&wayid=123123123"
>> [{ok,<<" this is the payload for way 123123123">>},
>>  {ok,<<" this is the payload for way 123123123 node id 5000">>},
>>  {ok,<<" this is the payload for way 123123123 node id 5002">>}]

curl -H "Content-Type:text/plain" \
"http://127.0.0.1:10018/fetch/deg?zoom=14&lat=51&lon=52"
>>[{ok,<<" this is the payload for way tile  tile_deg level=\"14\" lat=\"51\" lon= \"52\"">>},
>> {ok,<<" this is the payload for way 123123123">>},
>> {ok,<<" this is the payload for way 123123123 node id 5000">>},
>> {ok,<<" this is the payload for way 123123123 node id 5002">>}]
```

In case of errors the curl flag -v is helpful.

```
./rel/trafficapp/bin/trafficapp stop
```
Note: 
The web port of the rest interface is currently defined in /rel/files/app.config

devrel
----------

Above we showed how to start a single node, but this isn't typically how this application is tested.  Instead, there is something called a _devrel_ that allows one to easily set up a local `N`-node cluster. By defauly `N` is 4. You can change this by either editing the `Makefile` and changing `DEVNODES` to your prefered `N`, or by setting the variable `DEVNODES` before running the commands below.

Build the dev-nodes:

    make devrel

This did create 4 separate instances under the `dev/` dir; check it out:

    ls dev

Now, lets start all the nodes:

    for d in dev/dev*; do $d/bin/trafficapp start; done

Verify that the nodes are up and running:

    for d in dev/dev*; do $d/bin/trafficapp ping; done

You should see four `pong` replies.  At this point it is worth saying that you have four **INDIVIDUAL** trafficapp nodes running.  They are **NOT** aware of each other yet. In order to form a cluster you have to _join_ the nodes. That has to be done only once. If a node, or the entire cluster, goes down it will remember the nodes it was connected to.

    for d in dev/dev{2,3,4}; do $d/bin/trafficapp-admin join trafficapp1@127.0.0.1; done

Finally, to make sure they really all agree on the shape of the cluster you can ask if the _ring_ is "ready."

    ./dev/dev1/bin/trafficapp-admin ringready

Which returns something like:

    TRUE All nodes agree on the ring ['trafficapp1@127.0.0.1','trafficapp2@127.0.0.1','trafficapp3@127.0.0.1'],'trafficapp4@127.0.0.1']

To verify you have a 4 node cluster and to see the distribution of the ring, run the `member_status` command:

    ./dev/dev1/bin/trafficapp-admin member_status

Which returns:

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      25.0%      --      'trafficapp1@127.0.0.1'
    valid      25.0%      --      'trafficapp2@127.0.0.1'
    valid      25.0%      --      'trafficapp3@127.0.0.1'
    valid      25.0%      --      'trafficapp4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Pretty cool!! Your riak-core cluster is up and running.

And in case you want to stop all the nodes:

    for d in dev/dev*; do $d/bin/trafficapp stop; done

stage and stagedevrel
----------

For rapid, iterative development both `make rel` and `make devrel`
have a `stage` counterpart. Running either:

    make stage

or

    make stagedevrel

will cause the `deps` and `apps` directories to be symlinked into the
release(s) `lib` directory. This means you can edit your apps source
code, recompile it, and load it on to the running node(s). You can
either start mochiweb reloader

    reloader:start().

when attached to the node(s), which will cause any changes to be
automatically reloaded. Or attach to a running node and run

    `l(mod_name)`.

to reload a specific module.

