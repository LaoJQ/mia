-module(mia_upgrade).

-export([
         check_upgrade/0,
         do_upgrade/0,
         set_schema_version/0
        ]).

-define(SCHEMA_VERSION_FILENAME, "schema_version").

check_upgrade() ->
    NewVersion = new_schema_version(),
    case old_schema_version() of
        {error, enoent} ->
            {true, NewVersion};
        {ok, [NewVersion]} ->
            false;
        {ok, [_]} ->
            {true, NewVersion};
        Other ->
            exit({error, {"read schema_version file error.", Other}})
    end.

schema_version_file() ->
    filename:join([mnesia:system_info(directory), ?SCHEMA_VERSION_FILENAME]).

old_schema_version() ->
    file:consult(schema_version_file()).

new_schema_version() ->
    TableDefModule = mia_misc:mia_table_def_module(),
    TableDefList = TableDefModule:definitions(),
    mia_misc:md5_term(TableDefList).

do_upgrade() ->
    upgrade_backup(),
    %% TODO do real upgrade
    io:format("*** do upgrade~n"),
    del_backup(),
    %% set_schema_version().
    io:format("*** set schema version~n"),
    ok.

upgrade_backup() ->
    case copy_db() of
        [] ->
            io:format("*** upgrades: Mnesia dir backed up. ~n");
        Err ->
            exit({could_not_back_up_mnesia_dir, Err})
    end.

mnesia_dir() ->
    mnesia:system_info(directory).

backup_dir() ->
     mnesia_dir() ++ "-upgrade-backup".

copy_db() ->
    SrcDir = mnesia_dir(),
    DestDir = backup_dir(),
    os:cmd("cp -rf " ++ SrcDir ++ " " ++ DestDir).

del_backup() ->
    os:cmd("rm -rf " ++ backup_dir()).

set_schema_version() ->
    set_schema_version(new_schema_version()).
set_schema_version(Version) ->
    file:write_file(schema_version_file(), [io_lib:format("~p.~n", [Version])]).

