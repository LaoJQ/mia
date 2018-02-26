-module(mia_misc).

-export([
         get_env/1, get_env/2,
         start/1,
         do_with_time_cost/3
        ]).

get_env(Env) ->
    get_env(Env, undefined).

get_env(Env, Default) ->
    application:get_env(mia, Env, Default).

start(App) ->
    start_ok(App, application:start(App)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

do_with_time_cost(Fun, PrintStr, PrintArgs) ->
    T1 = erlang:monotonic_time(),

    Fun(),

    T2 = erlang:monotonic_time(),
    Cost = erlang:convert_time_unit(T2 - T1, native, millisecond),
    io:format(PrintStr ++ "Cost: ~p millisecond~n", PrintArgs ++ [Cost]).
