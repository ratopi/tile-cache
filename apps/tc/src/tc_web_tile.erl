%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%% Delivers a tile from the cache to the web client.
%%% @end
%%% Created : 22. Mär 2024 20:57
%%%-------------------------------------------------------------------
-module(tc_web_tile).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([init/2, info/3]).

-define(DELIVER_TIMEOUT, 30 * 1000).

%%====================================================================
%% cowboy callback
%%====================================================================

init(Req = #{headers := Headers, bindings := #{x := X, y := Y, z := Z}}, State) ->
	tc_tile_cache:deliver(self(), Headers, Z, X, Y),
	{cowboy_loop, Req, State, ?DELIVER_TIMEOUT}.

%%====================================================================
%% data callback
%%====================================================================

info(eof, Req, State) ->
	{stop, Req, State};

info({headers, Headers}, Req, State) ->
	NewReq = cowboy_req:stream_reply(200, Headers, Req),
	{ok, NewReq, State};

info({data, Data}, Req, State) ->
	ok = cowboy_req:stream_body(Data, nofin, Req),
	{ok, Req, State};

info(_Msg, Req, State) ->
	{ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
