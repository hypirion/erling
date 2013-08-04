-module(erling_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%==============================================================================
%% API functions
%%==============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

init([]) ->
    {ok, {{rest_for_one, 1, 0},
          [{erling_plugin_sup,
            {erling_plugin_sup, start_link, []},
            permanent,
            5000,
            supervisor,
            [erling_plugin_sup]},
           {erling_irc_server_sup,
            {erling_irc_server_sup, start_link, []},
            permanent,
            5000,
            supervisor}]}}.

