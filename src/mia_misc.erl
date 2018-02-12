-module(mia_misc).

-export([
         get_env/1, get_env/2
        ]).

get_env(Env) ->
    get_env(Env, undefined).

get_env(Env, Default) ->
    application:get_env(mia, Env, Default).

