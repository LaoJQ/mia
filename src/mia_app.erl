%%%-------------------------------------------------------------------
%% @doc mia public API
%% @end
%%%-------------------------------------------------------------------

-module(mia_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mia_init:init(),
    mia_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

