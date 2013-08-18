%%%-----------------------------------------------------------------------------
%%% File    : erling_irc_conv.erl
%%% Author  : Jean Niklas L'orange <jeannikl@hypirion.com>
%%% Description : Erling's plugin supervisor.
%%%
%%% Created :  16 Aug 2013 by Jean Niklas L'orange <jeannikl@hypirion.com>
%%%-----------------------------------------------------------------------------
-module(erling_irc_conv).

%%------------------------------------------------------------------------------
%% External exports
-export([message_to_tuple/1]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(SPACE, 16#20).
-define(CRLF, [16#0D, 16#0A]).
-define(IS_LETTER(Char),
        (($a =< Char andalso Char =< $z) orelse ($A =< Char andalso Char =< $Z))).
-define(IS_DIGIT(Char), ($0 =< Char andalso Char =< $9)).

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

%%==============================================================================
%% Internal functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: is_letter/1
%% Description: Returns true if the char is a letter, according to the ABNF
%%   syntax in rfc2812. See also the internal macro.
%% Returns: boolean()
%%------------------------------------------------------------------------------
is_letter(Char) when $A =< Char, Char =< $Z ->
    true;
is_letter(Char) when $a =< Char, Char =< $z ->
    true;
is_letter(_) ->
    false.

%%------------------------------------------------------------------------------
%% Function: is_digit/1
%% Description: Returns true if the char is a digit, according to the ABNF
%%   syntax in rfc2812. See also the internal macro.
%% Returns: boolean()
%%------------------------------------------------------------------------------
is_digit(Char) when $0 =< Char, Char =< $9 ->
    true;
is_digit(_) ->
    false.

%%------------------------------------------------------------------------------
%% Function: split_at/2
%% Description: Splits the message whenever encountering an element from Elems
%%   in the Message, or {Message, []} if none of the elements in Message are in
%%   Elems.
%% Returns: {BeforeMatch, AfterMatch} |
%%          {Message, []}
%%------------------------------------------------------------------------------
split_at(Elems, Message) ->
    Pred = fun(Elem) -> not lists:member(Elem, Elems) end,
    {Head, Tail} = lists:splitwith(Pred, Message),
    case Tail =:= [] of
        true -> {Message, []};
        false -> {Head, tl(Tail)}
    end.

%%------------------------------------------------------------------------------
%% Function: get_command/1
%% Description: Returns the command as an atom along with the rest of the
%%   message, according to the ABNF syntax in rfc2812. The special command
%%   `err` is an error in the message, not an actual command.
%% Returns: {CommandAtom, Rest} |
%%          {err, Reason}
%%------------------------------------------------------------------------------
get_command([F | Message]) ->
    if ?IS_LETTER(F) ->
            %% TODO: Convert first part to atom here.
            lists:splitwith(fun is_letter/1, [F | Message]);
       ?IS_DIGIT(F) ->
            [S, T | Rest] = Message,
            if ?IS_DIGIT(S), ?IS_DIGIT(T) ->
                    {lookup_command_name([F, S, T]), Rest};
               false -> % S or T not digit
                    {err, "First element was digit, but not second/third."}
            end;
       false -> % neither digit nor letter
            {err, "First character was neither digit not letter."}
    end.

%%------------------------------------------------------------------------------
%% Function: lookup_command_name/1
%% Description: Returns the command atom of the command code, a three-digit
%%   string. If the command code is not described in rfc2812, it will return
%%   the atom `notfound` instead.
%% Returns: CommandAtom |
%%          notfound
%%------------------------------------------------------------------------------
lookup_command_name(CommandCode) ->
    {error, "Not yet implemented"}.

%%------------------------------------------------------------------------------
%% Function: parse_message/1
%% Description: The internal handler for message_to_tuple.
%% Returns: {ok, {Prefix, Command, Params}} |
%%          {error, Reason}
%%------------------------------------------------------------------------------
parse_message([$: | Message]) ->
    {Prefix, Rest} = split_at(?SPACE, Message),
    {error, "Not yet implemented."};
parse_message(Message) ->
    get_command(Message),
    {error, "Not yet implemented."}.
