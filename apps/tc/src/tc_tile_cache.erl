%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tc_tile_cache).

-behaviour(gen_server).

-export([deliver/5]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(tile_cache_state, {cache_dir = <<"/tmp/tile-cache/">>, tile_server_url = <<"https://tile.openstreetmap.org/">>}).

-define(HTTP_TIMEOUT, 30 * 1000).

-record(request, {cache_dir, tile_server_url, message_ref, client_pid, client_headers, filepath, filepath_info, filepath_last_access, coordinates_path, z, x, y, url}).

%%%===================================================================
%%% API
%%%===================================================================

deliver(ClientPid, ClientHeaders, Z, X, Y) ->
	gen_server:call(?SERVER, {deliver, ClientPid, ClientHeaders, Z, X, Y}).

%%%===================================================================
%%% spawning
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server implementation
%%%===================================================================

init([]) ->
	{ok, #tile_cache_state{}}.



handle_call({deliver, ClientPid, ClientHeaders, Z, X, Y}, _From, State = #tile_cache_state{cache_dir = CacheDir, tile_server_url = TileServerUrl}) ->
	Ref = make_ref(),
	spawn(
		fun() ->
			inner_deliver(set_filepaths(#request{cache_dir = CacheDir, tile_server_url = TileServerUrl, message_ref = Ref, client_pid = ClientPid, client_headers = ClientHeaders, z = Z, x = X, y = Y}))
		end
	),
	{reply, {ok, Ref}, State};

handle_call(_Request, _From, State = #tile_cache_state{}) ->
	{reply, ok, State}.



handle_cast(_Request, State = #tile_cache_state{}) ->
	{noreply, State}.



handle_info(_Info, State = #tile_cache_state{}) ->
	{noreply, State}.



terminate(_Reason, _State = #tile_cache_state{}) ->
	ok.



code_change(_OldVsn, State = #tile_cache_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% internal functions
%%%===================================================================

set_filepaths(Request = #request{x = X, y = Y, z = Z, cache_dir = CacheDir}) ->
	Dir1 = <<CacheDir/binary, Z/binary>>,
	Dir2 = <<Dir1/binary, $/, X/binary>>,

	file:make_dir(Dir1),
	file:make_dir(Dir2),

	Path = <<Z/binary, $/, X/binary, $/, Y/binary>>,
	Filepath = <<CacheDir/binary, Path/binary>>,
	InfoFilepath = <<Filepath/binary, ".info">>,
	LastAccessFilepath = <<Filepath/binary, ".last">>,

	Request#request{coordinates_path = Path, filepath = Filepath, filepath_info = InfoFilepath, filepath_last_access = LastAccessFilepath}.



inner_deliver(Request = #request{}) ->
	case deliver_file(Request) of
		ok ->
			ok;
		not_found ->
			deliver_url(Request)
	end.



deliver_file(Request = #request{filepath = Filepath, filepath_info = InfoFilepath}) ->
	case file:open(Filepath, [read, raw, binary]) of
		{error, _} ->
			not_found;
		{ok, IO} ->
			io:fwrite("~p from cache~n", [Filepath]),
			spawn(fun() -> set_last_access(Request) end),
			{ok, [Headers | _]} = file:consult(InfoFilepath),
			send_to_client(Request, {headers, Headers}),
			deliver_file_content(IO, Request),
			file:close(IO)
	end.



deliver_file_content(IO, Request = #request{}) ->
	case file:read(IO, 1024) of
		{ok, Data} ->
			send_to_client(Request, {data, Data}),
			deliver_file_content(IO, Request);
		eof ->
			send_to_client(Request, eof);
		Err = {error, _} ->
			Err
	end.



deliver_url(Request = #request{tile_server_url = TileServerUrl, filepath = Filepath, coordinates_path = Path, client_headers = ClientHeaders}) ->
	{ok, IO} = file:open(Filepath, [write, raw, binary]),

	%%% https://tile.openstreetmap.org/{z}/{x}/{y}.png>>,
	% Url = <<"https://tile.openstreetmap.org/", Path/binary>>,
	Url = <<TileServerUrl/binary, Path/binary>>,

	ClientHeadersMapped = lists:map(
		fun({K, V}) ->
			{binary_to_list(K), V}
		end,
		maps:to_list(maps:remove(<<"host">>, ClientHeaders))
	),

	case httpc:request(get, {Url, ClientHeadersMapped}, [], [{sync, false}, {stream, self}, {body_format, binary}]) of
		{ok, RequestId} ->
			R = repacker(IO, RequestId, Request#request{url = Url}),
			io:fwrite("~p now in cache~n", [Filepath]),
			file:close(IO)
	end.



repacker(IO, RequestId, Request = #request{}) ->
	receive
		{http, {RequestId, stream_start, Headers}} ->
			ConvertedHeaders = convert_headers(Headers),
			send_to_client(Request, {headers, ConvertedHeaders}),
			spawn(
				fun() ->
					set_last_access(Request),
					Info = #{loaded_at => erlang:universaltime(), url => Request#request.url},
					file:write_file(Request#request.filepath_info, io_lib:format("~p.~n~p.~n", [ConvertedHeaders, Info]))
				end
			),
			repacker(IO, RequestId, Request);

		{http, {RequestId, stream, BinBodyPart}} ->
			send_to_client(Request, {data, BinBodyPart}),
			file:write(IO, BinBodyPart),
			repacker(IO, RequestId, Request);

		{http, {RequestId, stream_end, _Headers}} ->
			send_to_client(Request, eof)

	after ?HTTP_TIMEOUT ->
		send_to_client(Request, timeout),
		timeout
	end.



convert_headers(HeaderList) ->
	lists:foldl(
		fun({K, V}, M) ->
			maps:put(list_to_binary(K), list_to_binary(V), M)
		end,
		#{},
		HeaderList
	).



send_to_client(#request{message_ref = Ref, client_pid = ClientPid}, Msg) ->
	ClientPid ! {Ref, Msg}.



set_last_access(#request{filepath_last_access = LastAccessFilepath}) ->
	Now = os:system_time(millisecond),
	file:write_file(LastAccessFilepath, <<Now:64>>).
