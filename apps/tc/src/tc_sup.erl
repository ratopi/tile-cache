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

	Spec =
		#{
			strategy => one_for_one
		},

	Children = [
		#{
			id => tc_tile_cache,
			start => {tc_tile_cache, start_link, []},
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
