-module(catalogue).

%% Include files

-include_lib("kernel/include/logger.hrl").
-include("catalogue.hrl").

%% Exported functions

-export([
    start/0,
    stop/0,
    restart/0,
    guid/1,
    key/1,
    view/3,
    view/4,
    subscribe/0,
    select/2,
    select/3
]).

-export_type([
    key/0,
    view/0,
    guid/0
]).

-type key() :: atom().
-type view() :: atom().
-type guid() :: aplib:ulong().

%% API

start() ->
    aplib:start_app_recursive(catalogue).

stop() ->
    application:stop(catalogue).

restart() ->
    stop(),
    start().

-spec view(Key, View, Parser) -> Value when
    Key :: key(),
    View :: view(),
    Parser :: fun((json:json_object()) -> T),
    Value :: T.

view(Key, View, Parser) ->
    [Db | _] = ecouch:dbs(),
    view(Db, Key, View, Parser).

-spec view(Db, Key, View, Parser) -> Value when
    Db :: atom(),
    Key :: key(),
    View :: view(),
    Parser :: fun((json:json_object()) -> T),
    Value :: T.

view(Db, Key, View, Parser) ->
    case catalogue_cache:lookup(Db, Key, View) of
        [{_, Value}] ->
            Value;
        [] ->
            List = ecouch:get(atom_to_binary(Key, latin1)),
            Object = case application:get_env(catalogue, selector, ?MODULE) of
                ?MODULE -> select(Db, Key, List);
                Callback -> Callback:select(Key, List)
            end,
            Value = try
                Parser(Object)
            catch
                _Error:Reason:Stacktrace ->
                    ?LOG_ERROR(#{what => catalogue_parse, card_key => Key}, #{stacktrace => Stacktrace, error => parse_error, reason => Reason}),
                    error({catalogue_parse_failed, Key, View})
            end,
            catalogue_cache:update_view(Db, Key, View, Value),
            Value
    end.

-spec guid(Key) -> Guid when
    Key :: key(),
    Guid :: guid().

guid(Key) ->
    catalogue_guid:from_key(Key).

-spec key(Guid) -> Key when
    Guid :: guid(),
    Key :: key().

key(Guid) ->
    catalogue_guid:to_key(Guid).

-spec subscribe() -> true.

subscribe() ->
    gproc:reg({p, l, ?CATALOGUE_TABLE}).

-spec select(Key, List) -> tuple() when
    Key :: key(),
    List :: [{key(), tuple()}].

select(Key, List) ->
    case [Obj || {_, Obj} <- List, Obj =/= undefined] of
        [Obj|_] -> Obj;
        _ -> erlang:error({catalogue_unknown_key, Key})
    end.

-spec select(Db, Key, List) -> tuple() when
    Db :: atom(),
    Key :: key(),
    List :: [{key(), tuple()}].

select(Db, Key, List) ->
    case [Obj || {DbName, Obj} <- List, Obj =/= undefined, DbName =:= Db] of
        [Obj|_] -> Obj;
        _ -> erlang:error({catalogue_unknown_key, Key})
    end.

%% Local functions
