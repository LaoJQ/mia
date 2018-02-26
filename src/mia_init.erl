-module(mia_init).

-export([
         init/0
        ]).

init() ->
    set_mnesia_dir(),
    case mnesia:system_info(use_dir) of
        true ->
            start_mnesia_and_wait();
        false ->
            case mia_misc:get_env(mia_schema) of
                true ->
                    ensure_mnesia_dir(),
                    case mnesia:create_schema([node()]) of
                        {error, Reason} ->
                            exit({error, Reason});
                        ok ->
                            ok
                    end;
                _ ->
                    ignore
            end,
            start_mnesia_and_wait(),
            create_tables()
    end.

set_mnesia_dir() ->
    case mia_misc:get_env(mia_dir) of
        undefined ->
            ok;
        MiaDir ->
            MnesiaDir = filename:join([MiaDir, atom_to_list(node())]),
            application:set_env(mnesia, dir, MnesiaDir)
    end.

ensure_mnesia_dir() ->
    MnesiaDir = mnesia:system_info(directory) ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            exit({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.

start_mnesia_and_wait() ->
    mia_misc:do_with_time_cost(
      fun() ->
              mnesia:start(),
              TableList = mnesia:system_info(tables),
              mnesia:wait_for_tables(TableList, infinity)
      end,
      "Start mnesia.   ", []).

create_tables() ->
    case mia_misc:get_env(mia_table_def_module) of
        undefined ->
            exit({error, {"mia_table_def_module isn't' provided"}});
        TableDefModule ->
            lists:foreach(fun(TabDef) ->
                                  case mnesia:create_table(TabDef) of
                                      {atomic, ok} ->
                                          ok;
                                      Err ->
                                          exit({error, {Err, TabDef}})
                                  end
                          end, TableDefModule:definitions())
    end.
