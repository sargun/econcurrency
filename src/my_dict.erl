-module(my_dict).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    set_key/3,
    read_key/2,
    delete_key/2,
    cas_val/5
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-type my_dict() :: pid().
-type key() :: term().
-type value() :: term().

-export_type([my_dict/0, key/0, value/0]).

-type state() :: #{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec(set_key(MyDict :: my_dict(), Key :: key(), Value :: value()) -> ok).
set_key(MyDict, Key, Value) ->
    gen_server:call(MyDict, {set_key, Key, Value}).

-spec(read_key(MyDict :: my_dict(), Key :: key()) -> {ok, value()} | not_found).
read_key(MyDict, Key) ->
    gen_server:call(MyDict, {read_key, Key}).

-spec(delete_key(MyDict :: my_dict(), Key :: key()) -> ok).
delete_key(MyDict, Key) ->
    gen_server:call(MyDict, {delete_key, Key}).

-spec(cas_val(MyDict :: my_dict(), Key :: key(), OldVal :: value(), NewValue :: value(), SetOnNotExists :: boolean()) -> boolean()).
cas_val(MyDict, Key, OldVal, NewVal, SetOnNotExists) ->
    gen_server:call(MyDict, {cas_val, Key, OldVal, NewVal, SetOnNotExists}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: my_dict()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init([]) -> {ok, state()}).
init([]) ->
    {ok, #{}}.

handle_call({set_key, Key, Value}, _From, State) ->
    handle_set_key(Key, Value, State);
handle_call({read_key, Key}, _From, State) ->
    handle_read_key(Key, State);
handle_call({delete_key, Key}, _From, State) ->
    handle_delete_key(Key, State);
handle_call({cas_val, Key, OldVal, NewVal, SetOnNotExists}, _From, State) ->
    handle_cas_val(Key, OldVal, NewVal, SetOnNotExists, State).


-spec(handle_cast(Request :: term(), State0 :: state()) -> {noreply, State0 :: state()}).
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: term, State0 :: state()) -> {noreply, State0 :: state()}).
handle_info(_Info, State) ->
    {noreply, State}.


-spec(terminate(Reason :: term(), State :: state()) -> ok).
terminate(_Reason, _State) ->
    ok.

-spec(code_change(OldVsn :: term(), State0 :: state(), Extra :: term()) -> {ok, State0 :: state()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(handle_set_key(Key :: key(), Value :: value(), State0 :: state()) -> {reply, ok, state()}).
handle_set_key(Key, Value, State0) ->
    {reply, ok, maps:put(Key, Value, State0)}.

-spec(handle_read_key(Key :: key(), State0 :: state()) -> {reply, {ok, value()} | not_found, state()}).
handle_read_key(Key, State0) ->
    case maps:find(Key, State0) of
        {ok, Value} ->
            {reply, {ok, Value}, State0};
        error ->
            {reply, not_found, State0}
    end.

-spec(handle_delete_key(Key :: key(), State0 :: state()) -> {reply, ok, state()}).
handle_delete_key(Key, State0) ->
    {reply, ok, maps:remove(Key, State0)}.

-spec(handle_cas_val(Key :: key(), OldVal :: value(), NewVal :: value(), SetOnNotExists :: boolean(), State0 :: state()) -> {reply, boolean(), state()}).
handle_cas_val(Key, OldVal, NewVal, SetOnNotExists, State0) ->
    case maps:find(Key, State0) of
        {ok, OldVal} ->
            {reply, true, maps:put(Key, NewVal, State0)};
        error when SetOnNotExists == true ->
            {reply, true, maps:put(Key, NewVal, State0)};
        _ ->
            %% Either the key is not found, or the value is a different value
            {reply, false, State0}
    end.
