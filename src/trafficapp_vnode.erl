-module(trafficapp_vnode).
-behaviour(riak_core_vnode).
-include("trafficapp.hrl").

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
  init/1,
  terminate/2,
  handle_command/3,
  is_empty/1,
  delete/1,
  handle_handoff_command/3,
  handoff_starting/2,
  handoff_cancelled/1,
  handoff_finished/2,
  handle_handoff_data/2,
  encode_handoff_item/2,
  handle_coverage/4,
  handle_exit/3]).

-ignore_xref([
  start_vnode/1
]).

-record(state, {partition, basedir = "trafficapp_data"}).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  St = #state{partition = Partition},
  Base = make_base_path(St),
  %% filelib:ensure_dir/1 you make me so sad. You won't make the parents
  %% unless there's a last element which need not exist and will not be
  %% created. Crazytown, baby.
  case filelib:ensure_dir(Base ++ "/dummy") of
    ok -> {ok, St};
    {error, Reason} -> {error, Reason}
  end.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
  {reply, {pong, State#state.partition}, State};

handle_command({RequestId, {update, TileRec, Data}}, _Sender, State) ->
  BasePath = make_base_path(State),
  make_filename_and_path("reference", TileRec, ".meta", State), % creates string of Basepath/Tilepath/Subpath/Reference.meta

  SubPath = make_sub_path(TileRec),
  FullPath = make_full_path(BasePath, SubPath),
  filelib:ensure_dir(FullPath ++ "/"), % Add "/" to the end of ensure dir, in order to make ensure_dir work correctly
  FilenameMetaWithPath = FullPath ++ "/" ++ TileRec#num.tile_reference ++ ".meta",
  NewVersion = case filelib:is_regular(FilenameMetaWithPath) of
                 true ->
                   OldMD = read_metadata_file(FilenameMetaWithPath),
                   OldMD#num.version + 1;
                 false ->
                   1
               end,
  {MetaResult, DataResult, Loc} = update(State, TileRec#num{version = NewVersion}, Data),
  {reply, {RequestId, {MetaResult, DataResult, Loc}}, State};

handle_command({fetch, TileRec}, _Sender, State) ->
  BasePath = make_base_path(State),
  %%SubPath = make_sub_path(TileRec),
  %%FullPath = make_full_path(BasePath, SubPath),
  %%FilenameMetaWithPath = FullPath ++ "/" ++ TileRec#num.tile_reference ++ ".meta",


  MetaDataFileList = generate_meta_data_file_list(BasePath, "*", TileRec#num.tile_sub_list),
  DataList = lists:map(fun(FilenameMetaWithPath) ->
    ?PRINT({filenameMetaWithPath, FilenameMetaWithPath}),
    case filelib:is_regular(FilenameMetaWithPath) of
      true ->
        {ok, MD} = file:read_file(FilenameMetaWithPath),
        get_data(State, binary_to_term(MD));
      false ->
        not_found
    end
  end, MetaDataFileList),
  {reply, {MetaDataFileList, DataList}, State};

handle_command(Message, _Sender, State) ->
  ?PRINT({unhandled_command, Message}),
  {noreply, State}.

%% The `VisitFun' is riak_core_handoff_sender:visit_item/3
%% visit_item/3 is going to do all of the hard work of taking your serialized
%% data and pushing it over the wire to the remote node.
%%
%% Acc0 here is the internal state of the handoff. visit_item/3 returns an
%% updated handoff state, so you should use that in your own fold over
%% vnode data elements.
%%
%% The goal here is simple: for each vnode, find all objects, then for
%% each object in a vnode, grab its metadata and the file contents, serialize it
%% using the `encode_handoff_item/2' callback and ship it over to the
%% remote node.
%%
%% The remote node is going to receive the serialized data using
%% the `handle_handoff_data/2' function below.

%% handle_handoff_command(_Message, _Sender, State) ->
%%   {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun = VisitFun, acc0 = Acc0}, _Sender, State) ->
  AllObjects = get_all_objects(State),
  Base = make_base_path(State),

  Do = fun(Object, AccIn) ->
    MPath = path_from_object(Base, Object, ".meta"),
    ?PRINT(MPath),
    Meta = get_metadata(MPath),
    ?PRINT(Meta),
    %% TODO: Get all file versions
    {ok, LatestFile} = get_data(State, Meta),
    ?PRINT(LatestFile),
    %% This VisitFun expects a {Bucket, Key} pair
    %% but we don't have "buckets" in our application
    %% So we will just use our KEY macro from trafficapp.hrl
    %% and ignore it in the encoding.
    AccOut = VisitFun(?KEY(Meta#num.tile_md5), {Meta, LatestFile}, AccIn),
    ?PRINT(AccOut),
    AccOut
  end,
  Final = lists:foldl(Do, Acc0, AllObjects),
  {reply, Final, State};

handle_handoff_command(Message, _Sender, State) ->
  ?PRINT({unhandled_handoff_command, Message}),
  {noreply, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

%% handle_handoff_data(_Data, State) ->
%%   {reply, ok, State}.

handle_handoff_data(Data, State) ->
  {Meta, Blob} = binary_to_term(Data),
  R = case Meta#num.csum =:= erlang:adler32(Blob) of
        true ->
          Result = update(State, Meta, Blob),
          ?PRINT(Result),
          ok;
        false ->
          {error, file_checksum_differs}
      end,
  {reply, R, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

%% encode_handoff_item(_Key, Data = {_Meta, _File}) ->
%%   term_to_binary(Data).

is_empty(State) ->
  Result = case list_dir(State) of
             [] -> true;
             {error, _Reason} -> true;
             _ -> false
           end,
  {Result, State}.

delete(State) ->
  {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Private API


generate_meta_data_file_list(Path, ReferenceString, Tile_sub_list) ->
  case Tile_sub_list of
    [] ->
      filelib:wildcard(filename:join(Path, ReferenceString ++ ".meta")) ++
        filelib:wildcard(filename:join([Path, "*", ReferenceString ++ ".meta"])) ++
        filelib:wildcard(filename:join([Path, "*", "*", ReferenceString ++ ".meta"]));
    [OSMWayIDString] ->
      filelib:wildcard(filename:join([Path, "W_" ++ OSMWayIDString, ReferenceString ++ ".meta"])) ++
      filelib:wildcard(filename:join([Path, "W_" ++ OSMWayIDString, "*", ReferenceString ++ ".meta"]));
    [OSMWayIDString, OSMNodeIDString] ->
      ?PRINT({wildcard, {filename:join([Path, "W_" ++ OSMWayIDString, "N_" ++ OSMNodeIDString, ReferenceString ++ ".meta"])}}),
      filelib:wildcard(filename:join([Path, "W_" ++ OSMWayIDString, "N_" ++ OSMNodeIDString, ReferenceString ++ ".meta"]))
  end.


make_sub_path(TileRec) when is_record(TileRec, num) ->
  make_sub_path(TileRec#num.tile_sub_list);
make_sub_path([]) ->
  "";
make_sub_path([OSMWayIDString]) ->
  "W_" ++ OSMWayIDString;

make_sub_path([OSMWayIDString, OSMNodeIDString]) ->
  filename:join("W_" ++ OSMWayIDString, "N_" ++ OSMNodeIDString).

make_full_path(BasePath, SubPath) ->
  BasePath ++ "/" ++ SubPath.

make_filename_and_path(Filename, TileRec, Ending, State) ->
  Path = make_directory_path(TileRec, State),
  filename:join(Path, (Filename ++ Ending)).

make_directory_path(TileRec, State) ->
  BasePath = make_base_path(State),
  SubPath = make_sub_path(TileRec),
  FullPath = make_full_path(BasePath, SubPath).

read_metadata_file(FilenameWithPath) ->
  {ok, Data} = file:read_file(FilenameWithPath),
  binary_to_term(Data).

get_all_objects(State) ->
  [strip_meta(F) || F <- filelib:wildcard("*.meta", make_base_path(State))].

strip_meta(Filename) ->
  Index = string:chr(Filename, $.),
  string:substr(Filename, 1, Index - 1).

list_dir(State) ->
  case file:list_dir(make_base_path(State)) of
    {ok, Files} -> Files;
    Other -> Other
  end.

path_from_object(Base, Object, Suffix) ->
  File = Object ++ Suffix,
  filename:join([Base, File]).

get_metadata(State = #state{}, #num{tile_dir_path = Path}) ->
  get_metadata(State, Path);
get_metadata(State = #state{}, Path) when is_list(Path) ->
  MDPath = make_metadata_path(State, Path),
  get_metadata(MDPath).

get_metadata(MetaDataPath) ->
  {ok, Data} = file:read_file(MetaDataPath),
  binary_to_term(Data).

get_data(State = #state{}, R = #num{csum = Csum}) ->
  ?PRINT({get_data_file_to_read, make_versioned_file_path(State, R)}),
  {ok, Data} = file:read_file(make_versioned_file_path(State, R)),
  case Csum =:= erlang:adler32(Data) of
    true -> {ok, Data};
    %% false -> {ok, Data} % TODO proper checksumming
    false -> {error, file_checksum_differs, {Csum, erlang:adler32(Data)}}
  end.

make_metadata_path(State = #state{}, #num{tile_dir_path = Path}) ->
  make_metadata_path(State, Path);
make_metadata_path(State = #state{}, Path) when is_list(Path) ->
  filename:join([make_base_path(State), make_metadata_filename(Path)]).

make_versioned_file_path(State = #state{}, #num{tile_dir_path = Path, tile_sub_list = Tile_sub_list, version = V}) ->
  filename:join([make_base_path(State), make_sub_path(Tile_sub_list), make_versioned_filename("reference", V)]).

make_base_path(#state{partition = P, basedir = Base}) ->
  filename:join([Base, integer_to_list(P)]).

update(State = #state{}, TileRec, Blob) ->
  Base = make_base_path(State),
  Res0 = store_meta_file(make_filename_and_path("reference", TileRec, ".meta", State), TileRec),
  Res1 = store_file(make_filename_and_path("reference", TileRec, "." ++ integer_to_list(TileRec#num.version), State), Blob),
  {Res0, Res1, make_filename_and_path("reference", TileRec, ".meta", State)}.

make_metadata_filename(Path) when is_list(Path) ->
  make_filename(Path) ++ ".meta".

make_filename(String) when is_list(String) ->
  String.

make_versioned_filename(Filename, Version) when is_integer(Version) ->
  Filename ++ "." ++ integer_to_list(Version).

store_meta_file(Loc, Rec) ->
  Bin = term_to_binary(Rec),
  store_file(Loc, Bin).

store_file(Loc, Data) ->
  ?PRINT({store_file, Loc, Data}),
  file:write_file(Loc, Data).

hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X])).
