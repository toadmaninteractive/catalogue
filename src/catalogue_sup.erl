-module(catalogue_sup).

-behaviour(supervisor).

%% Include files

%% Exported functions

-export([
    start_link/0
]).

%% supervisor callbacks

-export([
    init/1
]).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', supervisor:startlink_err()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

init(_Args) ->
    GuidSpec = {
        catalogue_guid,
        {catalogue_guid, start_link, []},
        permanent, 2000, worker,
        [catalogue_guid]
    },
    CacheSpec = {
        catalogue_cache,
        {catalogue_cache, start_link, []},
        permanent, 2000, worker,
        [catalogue_cache]
    },
    {ok, {{one_for_one, 3, 10}, [GuidSpec, CacheSpec]}}.

%% Local functions
