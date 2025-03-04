-module(catalogue_guid).

-behaviour(gen_server).

%% Include files

-include("catalogue.hrl").

%% Exported functions

-export([
    start_link/0,
    from_key/1,
    to_key/1,
    guid/1,
    prefetch/1
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

%% API

-spec start_link() ->
    {'ok', pid()} | {'error', {'already_started', pid()} | any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec guid(Name) -> Result when
    Name :: atom() | binary() | list(),
    Result :: non_neg_integer().

guid(Name) when is_atom(Name) ->
    guid(atom_to_list(Name));
guid(Name) when is_binary(Name) ->
    guid(binary_to_list(Name));
guid(Name) when is_list(Name) ->
    erlang:crc32(Name).

-spec to_key(Guid) -> Result when
    Guid :: non_neg_integer(),
    Result :: atom().

to_key(Guid) ->
    case ets:lookup(?CATALOGUE_GUID_TABLE, Guid) of
        [{Guid, Name}] -> Name;
        [] -> erlang:error({unknown_guid, Guid})
    end.

-spec from_key(Name) -> Result when
    Name :: atom() | binary() | list(),
    Result :: non_neg_integer().

from_key(Name) ->
    Guid = guid(Name),
    case ets:member(?CATALOGUE_GUID_TABLE, Guid) of
        false -> ets:insert(?CATALOGUE_GUID_TABLE, {Guid, Name});
        true -> ignore
    end,
    Guid.

-spec prefetch(Table :: atom()) -> true.

prefetch(Table) ->
    GuidNames = [{guid(Name), Name} || {Name, _} <- ets:tab2list(Table)],
    ets:insert(?CATALOGUE_GUID_TABLE, GuidNames).

%% gen_server callbacks

init(_Args) ->
    ets:new(?CATALOGUE_GUID_TABLE, [set, named_table, public]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions
