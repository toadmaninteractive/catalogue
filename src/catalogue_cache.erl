-module(catalogue_cache).

-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include("catalogue.hrl").

%% Exported functions

-export([
    start_link/0,
    lookup/3,
    clear/0,
    update_view/4
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | {'error', {'already_started', pid()} | any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec lookup(Db, Key, View) -> Result when
    Db :: atom(),
    Key :: atom(),
    View :: atom(),
    Result :: [{{Key, View}, Value :: tuple()}].

lookup(Db, Key, View) ->
    Items = ets:lookup(?CATALOGUE_TABLE, {Db, Key, View}),
    [{{K, V}, Val} || {{_D, K, V}, Val} <- Items].

-spec clear() -> 'ok'.

clear() ->
    ets:delete_all_objects(?CATALOGUE_TABLE).

-spec update_view(Db, Key, View, Value) -> 'ok' when
    Db :: atom(),
    Key :: atom(),
    View :: atom(),
    Value :: tuple().

update_view(Db, Key, View, Value) ->
    ets:insert(?CATALOGUE_TABLE, {{Db, Key, View}, Value}).

%% gen_server callbacks

init(_) ->
    ets:new(?CATALOGUE_TABLE, [set, named_table, public]),
    ecouch:subscribe(),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ecouch, Db, Action}, State) ->
    do_delete(Db, action_id(Action)),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

action_id({deleted, Id}) -> Id;
action_id({changed, Id, _}) -> Id.

do_delete(Db, Id) ->
    Key = binary_to_atom(Id, latin1),
    ets:match_delete(?CATALOGUE_TABLE, {{Db, Key, '_'}, '_'}),
    gproc:send({p, l, ?CATALOGUE_TABLE}, {catalogue, Key}),
    gproc:send({p, l, ?CATALOGUE_TABLE}, {catalogue, Db, Key}),
    ok.
