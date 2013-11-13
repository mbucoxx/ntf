-module(ntf_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Always reload new beam files
    reloader:start(),

    ntf_sup:start_link().

stop(_State) ->
    ok.
