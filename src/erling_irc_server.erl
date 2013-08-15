%%%-----------------------------------------------------------------------------
%%% File    : erling_irc_server.erl
%%% Author  : Jean Niklas L'orange <jeannikl@hypirion.com>
%%% Description : Erling irc client server. (Not an IRC server, but a client)
%%%
%%% Created :  3 Aug 2013 by Jean Niklas L'orange <jeannikl@hypirion.com>
%%%-----------------------------------------------------------------------------
-module(erling_irc_server).

-behaviour(gen_server).
%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% External exports
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% irc client server state
-record(state, {
  % irc nicks
  nicks = [],
  % irc server host
  host = <<>>,
  % irc server port
  port = 0,
  % timeout method
  timeout_method = {nil, nil, nil}
}).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%==============================================================================
%% External functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: start_link/4
%% Description: Starts the server
%% Argument explanations:
%%  Host - A binary with the hostname to connect to.
%%  Post - An integer with the port to connect to.
%%  Nicks - A list of binaries with nicknames to use. If none are available,
%%    will return an okay server, but crash immediately afterwards.
%%  TimeoutMethod - A tuple {Method, Wait, Retries}, where Method is either the
%%    atom constant or exponential. Wait is an integer, specifying the amount of
%%    time to wait on first reconnection attempt. Retries specifies how many
%%    times one should attempt to reconnect. Use infinity for infinitely many
%%    attempts.
%%------------------------------------------------------------------------------
%% TODO: Add in possibility to use SSL (socket option)
start_link(Host, Port, Nicks, TimeoutMethod) ->
    gen_server:start_link(?MODULE, [Host, Port, Nicks, TimeoutMethod], []).

%%==============================================================================
%% Server functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%------------------------------------------------------------------------------
init([Host, Port, Nicks, TimeoutMethod]) ->
    {ok, #state{host = Host, port = Port, nicks = Nicks,
                timeout_method = TimeoutMethod}}.

%%------------------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%------------------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%------------------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%------------------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%------------------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%------------------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%------------------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%%% Internal functions
%%------------------------------------------------------------------------------
