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
	% io:fwrite("~p~n", [Req]),
	% io:fwrite("~p~n", [Headers]),
	% io:fwrite(". ~p .~n", [X]),
	% io:fwrite(". ~p .~n", [Y]),
	% io:fwrite(". ~p .~n", [Z]),
	timer:send_after(1000, eof),
	tc_tile_cache:deliver(self(), Headers, Z, X, Y),
	{cowboy_loop, Req, State, ?DELIVER_TIMEOUT}.

%%====================================================================
%% data callback
%%====================================================================

info(eof, Req, State) ->
	{stop, Req, State};

info({headers, Headers}, Req, State) ->
	io:fwrite("h ~p~n", [Headers]),
	% io:fwrite("oldreq ~p~n", [Req]),
	NewReq = cowboy_req:stream_reply(200, Headers, Req),
	% io:fwrite("newreq ~p~n", [NewReq]),
	{ok, NewReq, State};

info({data, Data}, Req, State) ->
	% io:fwrite("d ~p~n", [size(Data)]),
	ok = cowboy_req:stream_body(Data, nofin, Req),
	{ok, Req, State};

info(_Msg, Req, State) ->
	{ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

callback() ->
	Pid = self(),
	fun(Msg) ->
		Pid ! Msg
	end.
