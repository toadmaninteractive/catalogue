-module(catalogue_app).

-behaviour(application).

%% Include files

%% Exported functions

-export([
    start/2,
    stop/1
]).

%% API

-spec start(Type, StartArgs) -> Result when
    Type :: 'normal' | {'takeover', node()} | {'failover', node()},
    StartArgs :: any(),
    Result :: {'ok', pid()} | 'ignore' | {'error', supervisor:startlink_err()}.

start(_Type, _StartArgs) ->
    case catalogue_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

stop(_State) ->
    ok.

%% Local functions
