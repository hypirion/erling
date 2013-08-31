# "Internal" Explanation of `erling_irc_conv.erl`

## `parse_prefix` and friends

This section will cover `parse_prefix` and the internal parsing/validation of a
prefix, according to the RFC 2812. Relevant ABNF definitions from RFC 2812:

```abnf
prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )

servername =  hostname
host       =  hostname / hostaddr
hostname   =  shortname *( "." shortname )
shortname  =  ( letter / digit ) [ *( letter / digit / "-" )
                                   ( letter / digit ) ]
hostaddr   =  ip4addr / ip6addr
ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
nickname   =  ( letter / special ) *8( letter / digit / special / "-" )

user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                  ; any octet except NUL, CR, LF, " " and "@"
letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
digit      =  %x30-39                 ; 0-9
hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
special    =  %x5B-60 / %x7B-7D
                 ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
```

The function header is defined as such:

```erl
%%------------------------------------------------------------------------------
%% Function: parse_prefix/1
%% Description: Parses a prefix and returns it in a more usable format.
%% Returns: {servername, Servername} |
%%          {nickname, [Nickname, User, Host]} |
%%          {undetermined, Name} |
%%          {error, Reason}
%%------------------------------------------------------------------------------
```

`parse_prefix` takes in a single argument, a string, which is the complete
prefix. No more, no less. The prefix will be converted into a tuple, in which
the first element contain the "what", and the second contain the contents. If
the result can *only* be a servername, the first element is `servername` and the
second element is the servername string. It should be equivalent with the input
string as of now.

If the result can *only* be a nickname, the first argument is `nickname`. The
second argument is a list of three elements: The nickname part, the user part
and the host part. The nickname is the actual nickname string, the user is the
username string the user has on the specified host, and the host is a host tuple
which will be explained later on.

If it's impossible to decide whether this is a servername or a nickname,
`undetermined` will be returned, with the same string as the one returned.

The prefix parsing is relatively straightforward. As a servername can contain
legal values a username can't and vice versa, we can keep track of which
possible types we can have. If the name can both be a nickname and a servername,
we keep it undetermined. The finite state machine for `parse_prefix` turns as
follows into this:

<p align="center">
  <img src="imgs/parse_prefix_fsm.png" alt="The parse_prefix FSM."/>
</p>
