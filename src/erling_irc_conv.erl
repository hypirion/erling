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

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

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
-spec message_to_tuple(nonempty_string()) -> {ok, {string(), string(), [any()]}}
                                                 | {error, nonempty_string()}.
message_to_tuple(Message) ->
    parse_message(Message).

%%==============================================================================
%% Internal functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: is_letter/1
%% Description: Returns true if the char is a letter, according to the ABNF
%%   syntax in rfc2812. See also the internal macro.
%% Returns: boolean()
%%------------------------------------------------------------------------------
-spec is_letter(char()) -> boolean().
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
-spec is_digit(char()) -> boolean().
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
-spec split_at(string(), M) -> {nonempty_string(), nonempty_string()}
                                   | {M, []} when M :: nonempty_string().
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
-spec get_command(nonempty_string()) -> {atom(), string()}
                                            | {err, nonempty_string()}.
get_command([F | Message]) ->
    if ?IS_LETTER(F) ->
            %% TODO: Convert first part to atom here.
            lists:splitwith(fun is_letter/1, [F | Message]);
       ?IS_DIGIT(F) ->
            case Message of
                [S, T | Rest] ->
                    if ?IS_DIGIT(S), ?IS_DIGIT(T) ->
                            {lookup_command_name([F, S, T]), Rest};
                       true -> % S or T not digit
                            {err, "First element was digit, but not second/third."}
                    end;
                _ ->
                    {err, "First element was digit, didn't get enough characters."}
            end;
       true -> % neither digit nor letter
            {err, "First character was neither digit not letter."}
    end.

%%------------------------------------------------------------------------------
%% Function: parse_message/1
%% Description: The internal handler for message_to_tuple.
%% Returns: {ok, {Prefix, Command, Params}} |
%%          {error, Reason}
%%------------------------------------------------------------------------------
-spec parse_message(string()) -> {ok, {string(), string(), any()}}
                                     | {error, nonempty_string()}.
parse_message([]) ->
    {error, "Empty string"};
parse_message([$: | Message]) ->
    {Prefix, Rest} = split_at([?SPACE], Message),
    case parse_message(Rest) of
        {ok, {"", Cmd, Params}} ->
            {ok, {Prefix, Cmd, Params}};
        {error, Reason} ->
            {error, Reason}
    end;
parse_message(Message) ->
    get_command(Message),
    {error, "Not yet implemented."}.

%%------------------------------------------------------------------------------
%% Function: lookup_command_name/1
%% Description: Returns the command atom of the command code, a three-digit
%%   string. If the command code is not described in rfc2812, it will return
%%   the atom `notfound` instead.
%% Returns: CommandAtom |
%%          notfound
%%------------------------------------------------------------------------------
-spec lookup_command_name(nonempty_string()) -> atom().
lookup_command_name("001") -> rpl_welcome;
lookup_command_name("002") -> rpl_yourhost;
lookup_command_name("003") -> rpl_created;
lookup_command_name("004") -> rpl_myinfo;
lookup_command_name("005") -> rpl_bounce;
lookup_command_name("302") -> rpl_userhost;
lookup_command_name("303") -> rpl_ison;
lookup_command_name("301") -> rpl_away;
lookup_command_name("305") -> rpl_unaway;
lookup_command_name("306") -> rpl_nowaway;
lookup_command_name("311") -> rpl_whoisuser;
lookup_command_name("312") -> rpl_whoisserver;
lookup_command_name("313") -> rpl_whoisoperator;
lookup_command_name("317") -> rpl_whoisidle;
lookup_command_name("318") -> rpl_endofwhois;
lookup_command_name("319") -> rpl_whoischannels;
lookup_command_name("314") -> rpl_whowasuser;
lookup_command_name("369") -> rpl_endofwhowas;
lookup_command_name("321") -> rpl_liststart;
lookup_command_name("322") -> rpl_list;
lookup_command_name("323") -> rpl_listend;
lookup_command_name("325") -> rpl_uniqopis;
lookup_command_name("324") -> rpl_channelmodeis;
lookup_command_name("331") -> rpl_notopic;
lookup_command_name("332") -> rpl_topic;
lookup_command_name("341") -> rpl_inviting;
lookup_command_name("342") -> rpl_summoning;
lookup_command_name("346") -> rpl_invitelist;
lookup_command_name("347") -> rpl_endofinvitelist;
lookup_command_name("348") -> rpl_exceptlist;
lookup_command_name("349") -> rpl_endofexceptlist;
lookup_command_name("351") -> rpl_version;
lookup_command_name("352") -> rpl_whoreply;
lookup_command_name("315") -> rpl_endofwho;
lookup_command_name("353") -> rpl_namreply;
lookup_command_name("366") -> rpl_endofnames;
lookup_command_name("364") -> rpl_links;
lookup_command_name("365") -> rpl_endoflinks;
lookup_command_name("367") -> rpl_banlist;
lookup_command_name("368") -> rpl_endofbanlist;
lookup_command_name("371") -> rpl_info;
lookup_command_name("374") -> rpl_endofinfo;
lookup_command_name("375") -> rpl_motdstart;
lookup_command_name("372") -> rpl_motd;
lookup_command_name("376") -> rpl_endofmotd;
lookup_command_name("381") -> rpl_youreoper;
lookup_command_name("382") -> rpl_rehashing;
lookup_command_name("383") -> rpl_youreservice;
lookup_command_name("391") -> rpl_time;
lookup_command_name("392") -> rpl_usersstart;
lookup_command_name("393") -> rpl_users;
lookup_command_name("394") -> rpl_endofusers;
lookup_command_name("395") -> rpl_nousers;
lookup_command_name("200") -> rpl_tracelink;
lookup_command_name("201") -> rpl_traceconnecting;
lookup_command_name("202") -> rpl_tracehandshake;
lookup_command_name("203") -> rpl_traceunknown;
lookup_command_name("204") -> rpl_traceoperator;
lookup_command_name("205") -> rpl_traceuser;
lookup_command_name("206") -> rpl_traceserver;
lookup_command_name("207") -> rpl_traceservice;
lookup_command_name("208") -> rpl_tracenewtype;
lookup_command_name("209") -> rpl_traceclass;
lookup_command_name("210") -> rpl_traceconnect;
lookup_command_name("261") -> rpl_tracelog;
lookup_command_name("262") -> rpl_traceend;
lookup_command_name("211") -> rpl_statslinkinfo;
lookup_command_name("212") -> rpl_statscommands;
lookup_command_name("219") -> rpl_endofstats;
lookup_command_name("242") -> rpl_statsuptime;
lookup_command_name("243") -> rpl_statsoline;
lookup_command_name("221") -> rpl_umodeis;
lookup_command_name("234") -> rpl_servlist;
lookup_command_name("235") -> rpl_servlistend;
lookup_command_name("251") -> rpl_luserclient;
lookup_command_name("252") -> rpl_luserop;
lookup_command_name("253") -> rpl_luserunknown;
lookup_command_name("254") -> rpl_luserchannels;
lookup_command_name("255") -> rpl_luserme;
lookup_command_name("256") -> rpl_adminme;
lookup_command_name("257") -> rpl_adminloc1;
lookup_command_name("258") -> rpl_adminloc2;
lookup_command_name("259") -> rpl_adminemail;
lookup_command_name("263") -> rpl_tryagain;
lookup_command_name("401") -> err_nosuchnick;
lookup_command_name("402") -> err_nosuchserver;
lookup_command_name("403") -> err_nosuchchannel;
lookup_command_name("404") -> err_cannotsendtochan;
lookup_command_name("405") -> err_toomanychannels;
lookup_command_name("406") -> err_wasnosuchnick;
lookup_command_name("407") -> err_toomanytargets;
lookup_command_name("408") -> err_nosuchservice;
lookup_command_name("409") -> err_noorigin;
lookup_command_name("411") -> err_norecipient;
lookup_command_name("412") -> err_notexttosend;
lookup_command_name("413") -> err_notoplevel;
lookup_command_name("414") -> err_wildtoplevel;
lookup_command_name("415") -> err_badmask;
lookup_command_name("421") -> err_unknowncommand;
lookup_command_name("422") -> err_nomotd;
lookup_command_name("423") -> err_noadmininfo;
lookup_command_name("424") -> err_fileerror;
lookup_command_name("431") -> err_nonicknamegiven;
lookup_command_name("432") -> err_erroneusnickname;
lookup_command_name("433") -> err_nicknameinuse;
lookup_command_name("436") -> err_nickcollision;
lookup_command_name("437") -> err_unavailresource;
lookup_command_name("441") -> err_usernotinchannel;
lookup_command_name("442") -> err_notonchannel;
lookup_command_name("443") -> err_useronchannel;
lookup_command_name("444") -> err_nologin;
lookup_command_name("445") -> err_summondisabled;
lookup_command_name("446") -> err_usersdisabled;
lookup_command_name("451") -> err_notregistered;
lookup_command_name("461") -> err_needmoreparams;
lookup_command_name("462") -> err_alreadyregistered;
lookup_command_name("463") -> err_nopermforhost;
lookup_command_name("464") -> err_passwdmismatch;
lookup_command_name("465") -> err_yourebannedcreep;
lookup_command_name("466") -> err_youwillbebanned;
lookup_command_name("467") -> err_keyset;
lookup_command_name("471") -> err_channelisfull;
lookup_command_name("472") -> err_unknownmode;
lookup_command_name("473") -> err_inviteonlychan;
lookup_command_name("474") -> err_bannedfromchan;
lookup_command_name("475") -> err_badchannelkey;
lookup_command_name("476") -> err_badchanmask;
lookup_command_name("477") -> err_nochanmodes;
lookup_command_name("478") -> err_banlistfull;
lookup_command_name("481") -> err_noprivileges;
lookup_command_name("482") -> err_chanoprivsneeded;
lookup_command_name("483") -> err_cantkillserver;
lookup_command_name("484") -> err_restricted;
lookup_command_name("485") -> err_uniqopprivsneeded;
lookup_command_name("491") -> err_nooperhost;
lookup_command_name("501") -> err_umodeunknownflag;
lookup_command_name("502") -> err_usersdontmatch;
lookup_command_name(_CommandCode) -> notfound. %% We could add in reserved too?

%%==============================================================================
%% Eunit and PropEr tests
%%==============================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% EUnit tests

is_digit_test() ->
    %% All digits should be, well, digits.
    ?assert(lists:all(fun is_digit/1, "0123456789")),
    ?assertNot(lists:any(fun is_digit/1, "Hello, some non-digits goin' around")).

is_letter_test() ->
    %% All letters should be letters.
    ?assert(lists:all(fun is_letter/1, "AllTheseAreLetterz")),
    ?assert(lists:all(fun is_letter/1, "THESETOOABCDEFGHIJKLMNOPQRSTUVWXYZ")),
    ?assertNot(lists:any(fun is_letter/1, "?!£$_1284`([~@%&](+))")).

lookup_command_test() ->
    %% Random lookups to ensure they work as intended.
    ?assertEqual(lookup_command_name("409"), err_noorigin),
    ?assertEqual(lookup_command_name("317"), rpl_whoisidle),
    ?assertEqual(lookup_command_name("210"), rpl_traceconnect),
    ?assertEqual(lookup_command_name("???"), notfound),
    ?assertEqual(lookup_command_name("00409"), notfound),
    ?assertEqual(lookup_command_name("??????"), notfound),
    ?assertEqual(lookup_command_name("999"), notfound).

%% Property sets

prop_digit_fun_macro_equal() ->
    ?FORALL(F, char(),
            begin is_digit(F) =:= ?IS_DIGIT(F) end).

prop_letter_fun_macro_equal() ->
    ?FORALL(F, char(),
            begin is_letter(F) =:= ?IS_LETTER(F) end).

proper_module_test() ->
    ?assertEqual(
       [],
       proper:check_specs(?MODULE, [long_result, verbose, {numtests, 1000}])),
    ?assertEqual(
       [],
       proper:module(?MODULE, [long_result, verbose, {numtests, 1000}])).

-endif.
