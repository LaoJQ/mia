-module(mia).

-export([
         start/0
        ]).

-export([
         dirty_read/2,
         dirty_index_read/3,
         dirty_write/2,
         dirty_delete/2,
         dirty_all_keys/1,
         dirty_first/1,
         dirty_last/1,
         dirty_next/2,
         dirty_prev/2
        ]).

-export([
         table_info/2,
         size/1
        ]).

-export([
         transaction/1,
         sync_transaction/1,
         sync_dirty/1,
         async_dirty/1
        ]).

start() ->
    application:start(mia).


dirty_read(Table, Key) ->
    async_dirty(fun() -> 
                        mnesia:read(Table, Key)
                end).

dirty_index_read(Table, IndexVal, IndexPos) ->
    async_dirty(fun() ->
                        mnesia:index_read(Table, IndexVal, IndexPos)
                end).

dirty_write(Table, Record) ->
    async_dirty(fun() -> 
                        mnesia:write(Table, Record, write)
                end).

dirty_delete(Table, Key) ->
    async_dirty(fun() -> 
                        mnesia:delete({Table, Key})
                end).

dirty_all_keys(Table) ->
    async_dirty(fun() ->
                        mnesia:all_keys(Table)    
                end).

dirty_first(Table) ->
    async_dirty(fun() -> 
                        mnesia:first(Table)
                end).

dirty_last(Table) ->
    async_dirty(fun() -> 
                        mnesia:last(Table)
                end).

dirty_next(Table, Key) ->
    async_dirty(fun() ->
                        mnesia:next(Table, Key)
                end).
dirty_prev(Table, Key) ->
    async_dirty(fun() ->
                        mnesia:prev(Table, Key)
                end).

table_info(Table, Item) ->
    async_dirty(fun() ->
                        mnesia:table_info(Table, Item)
                end).

size(Table) ->
    table_info(Table, size).


transaction(Fun) -> 
    mnesia:activity(transaction, Fun).

sync_transaction(Fun) -> 
    mnesia:activity(sync_transaction, Fun).

sync_dirty(Fun) -> 
    mnesia:activity(sync_dirty, Fun).

async_dirty(Fun) -> 
    mnesia:activity(async_dirty, Fun).
