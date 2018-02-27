-module(mia_misc).

-export([
         get_env/1, get_env/2,
         start/1,
         do_with_time_cost/3,
         mia_table_def_module/0,
         md5_term/1
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

mia_table_def_module() ->
    case mia_misc:get_env(mia_table_def_module) of
        undefined ->
            exit({error, {"mia_table_def_module isn't' provided"}});
        TableDefModule ->
            TableDefModule
    end.

md5_term(Term) ->
    to_hex(erlang:md5(io_lib:format("~w", [Term]))).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    list_to_binary(to_hex(binary_to_list(Bin)));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.
