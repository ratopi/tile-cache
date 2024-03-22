%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 22. Mär 2024 22:18
%%%-------------------------------------------------------------------
-module(tc_web_log).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([init/2]).

%%====================================================================
%% cowboy callback
%%====================================================================

init(Req = #{headers := Headers}, State) ->
	io:fwrite("~p~n", [Headers]),
	{ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
