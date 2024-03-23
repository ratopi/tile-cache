%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2024, Ralf Th. Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2024 15:49
%%%-------------------------------------------------------------------
-module(tc_web).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

%% API
-export([start_clear/1]).

%%====================================================================
%% Cowboy start
%%====================================================================

start_clear(Port) ->
	io:fwrite("starting cowboy at port ~p~n", [Port]),
	cowboy:start_clear(
		?MODULE,
		[
			{port, Port}
		],
		#{
			env => #{
				dispatch => get_cowboy_dispatch()
			}
		}
	).

%%====================================================================
%% Internal functions
%%====================================================================

get_cowboy_dispatch() ->
	Hosts = '_',

	Routes =
		[
			{"/LOG", tc_web_log, []},
			{"/:z/:x/:y", tc_web_tile, []},
			{"/", cowboy_static, {priv_file, tc, "static/index.html"}},
			{"/[...]", cowboy_static, {priv_dir, tc, "static"}}
		],

	cowboy_router:compile([{Hosts, Routes}]).

%%====================================================================
%% Internal functions
%%====================================================================
