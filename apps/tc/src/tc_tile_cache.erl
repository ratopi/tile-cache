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

-record(tile_cache_state, {cache_dir = <<"/tmp/">>, tile_server_url = <<"https://tile.openstreetmap.org/">>}).

-define(HTTP_TIMEOUT, 30 * 1000).

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



handle_call({deliver, ClientPid, ClientHeaders, Z, X, Y}, _From, State = #tile_cache_state{}) ->
	spawn(
		fun() ->
			inner_deliver(ClientPid, ClientHeaders, Z, X, Y)
		end
	),
	{reply, ok, State};

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

inner_deliver(ClientPid, ClientHeaders, Z, X, Y) ->

	Path = <<Z/binary, $/, X/binary, $/, Y/binary>>,

	Filename = <<"/tmp/tile-cache/", Path/binary>>,
	HeaderFilename = <<Filename/binary, ".headers">>,

	case deliver_file(ClientPid, Filename, HeaderFilename) of
		ok ->
			ok;

		{not_found, Filename} ->
			file:make_dir(<<"/tmp/tile-cache/", Z/binary>>),
			file:make_dir(<<"/tmp/tile-cache/", Z/binary, $/, X/binary>>),
			deliver_url(ClientPid, ClientHeaders, Filename, HeaderFilename, Path)

	end.



deliver_file(ClientPid, Filename, HeaderFilename) ->
	case file:open(Filename, [read, raw, binary]) of
		{error, _} ->
			{not_found, Filename};
		{ok, IO} ->
			io:fwrite("~p from cache~n", [Filename]),
			{ok, [Headers | _]} = file:consult(HeaderFilename),
			ClientPid ! {headers, Headers},
			deliver_file_content(IO, ClientPid),
			file:close(IO)
	end.



deliver_file_content(IO, ClientPid) ->
	case file:read(IO, 1024) of
		{ok, Data} ->
			ClientPid ! {data, Data},
			deliver_file_content(IO, ClientPid);
		eof ->
			ClientPid ! eof;
		Err = {error, _} ->
			Err
	end.



deliver_url(ClientPid, ClientHeaders, Filename, HeaderFilename, Path) ->
	{ok, IO} = file:open(Filename, [write, raw, binary]),

	%%% https://tile.openstreetmap.org/{z}/{x}/{y}.png>>,
	Url = <<"https://tile.openstreetmap.org/", Path/binary>>,

	ClientHeadersMapped = lists:map(
		fun({K, V}) ->
			{binary_to_list(K), V}
		end,
		maps:to_list(maps:remove(<<"host">>, ClientHeaders))
	),

	case httpc:request(get, {Url, ClientHeadersMapped}, [], [{sync, false}, {stream, self}, {body_format, binary}]) of
		{ok, RequestId} ->
			R = repacker(IO, HeaderFilename, RequestId, ClientPid),
			io:fwrite("~p now in cache~n", [Filename]),
			file:close(IO)
	end.



repacker(IO, HeaderFilename, RequestId, ClientPid) ->
	receive
		{http, {RequestId, stream_start, Headers}} ->
			ConvertedHeaders = convert_headers(Headers),
			ClientPid ! {headers, ConvertedHeaders},
			Info = #{loaded_at => erlang:universaltime()},
			file:write_file(HeaderFilename, io_lib:format("~p.~n~p.~n", [ConvertedHeaders, Info])),
			repacker(IO, HeaderFilename, RequestId, ClientPid);

		{http, {RequestId, stream, BinBodyPart}} ->
			ClientPid ! {data, BinBodyPart},
			file:write(IO, BinBodyPart),
			repacker(IO, HeaderFilename, RequestId, ClientPid);

		{http, {RequestId, stream_end, _Headers}} ->
			ClientPid ! eof

	after ?HTTP_TIMEOUT ->
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

