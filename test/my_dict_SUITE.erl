%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2017 3:45 PM
%%%-------------------------------------------------------------------
-module(my_dict_SUITE).
-author("sdhillon").

-define(KEY_COUNT, 10000).
-define(VALUE_SIZE, 4096).

%% API
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

-export([basic_test/1, try_to_leak_memory/1]).

all() ->
    [basic_test, {group, memory_leak_check}].

groups() ->
    [
        {memory_leak_check, [{repeat, 10}], [try_to_leak_memory]}
    ].

basic_test(_Config) ->
    {ok, MyDict} = my_dict:start_link(),
    not_found = my_dict:read_key(MyDict, <<"foo">>),
    my_dict:set_key(MyDict, <<"foo">>, <<"bar">>),
    {ok, <<"bar">>} = my_dict:read_key(MyDict, <<"foo">>),
    true = my_dict:cas_val(MyDict, <<"foo">>, <<"bar">>, <<"baz">>, false),
    false = my_dict:cas_val(MyDict, <<"foo">>, <<"bar">>, <<"baz">>, false),
    my_dict:delete_key(MyDict, <<"foo">>),
    not_found = my_dict:read_key(MyDict, <<"foo">>).

init_per_group(memory_leak_check, Config) ->
    TotalMemory =  erlang:memory(total),
    [{total_memory_start, TotalMemory}|Config].

try_to_leak_memory(_Config) ->
    {ok, MyDict} = my_dict:start_link(),
    use_up_memory(MyDict).

use_up_memory(MyDict) ->
    use_up_memory(MyDict, ?KEY_COUNT).

use_up_memory(_, 0) ->
    ok;
use_up_memory(MyDict, Iterations) ->
    ok = my_dict:set_key(MyDict, Iterations, <<0:?VALUE_SIZE>>),
    use_up_memory(MyDict, Iterations - 1).


end_per_group(memory_leak_check, Config) ->
    EndMemory =  erlang:memory(total),
    {_, StartMemory} = proplists:lookup(total_memory_start, Config),
    UsedMemory = EndMemory - StartMemory,
    true = UsedMemory < ((?KEY_COUNT * ?VALUE_SIZE) / 2),
    Config.
