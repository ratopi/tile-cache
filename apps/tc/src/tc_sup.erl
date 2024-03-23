%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tc_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%%===================================================================
%%% supervisor API
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callback
%%%===================================================================

init([]) ->
	{ok, Port} = application:get_env(port),
	{ok, CacheDir} = application:get_env(cache_dir),
	{ok, TileServerUrl} = application:get_env(tile_server_url),

	Spec =
		#{
			strategy => one_for_one
		},

	Children = [
		#{
			id => tc_tile_cache,
			start => {tc_tile_cache, start_link, [CacheDir, TileServerUrl]},
			type => worker
		},
		#{
			id => tc_web,
			start => {tc_web, start_clear, [Port]},
			type => worker
		}
	],

	{ok, {Spec, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
