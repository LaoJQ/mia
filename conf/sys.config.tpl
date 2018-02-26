%% -*- mode: erlang -*-

[
 {mia, [
        {mia_dir, "data/mnesia/"},
        {mia_schema, true},
        {mia_table_def_module, mia_table_def}
       ]},
 {mnesia, [
           {dump_log_write_threshold, 50000},
           {dc_dump_limit, 64}
          ]}
].
