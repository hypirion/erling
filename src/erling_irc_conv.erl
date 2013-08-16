%%%-----------------------------------------------------------------------------
%%% File    : erling_irc_conv.erl
%%% Author  : Jean Niklas L'orange <jeannikl@hypirion.com>
%%% Description : Erling's plugin supervisor.
%%%
%%% Created :  18 Aug 2013 by Jean Niklas L'orange <jeannikl@hypirion.com>
%%%-----------------------------------------------------------------------------
-module(erling_irc_conv).

%%------------------------------------------------------------------------------
%% External exports
-export([message_to_tuple/1]).

%%==============================================================================
%% External functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: message_to_tuple/1
%% Description: Converts a message to a command tuple, as defined by the grammar
%%  in rfc2812.
%% Returns: {ok, {Prefix, Command, Params}} |
%%          {error, Reason}
%%------------------------------------------------------------------------------
message_to_tuple(Message) ->
    {error, "Not yet implemented."}.
