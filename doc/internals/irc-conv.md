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

### User and Host Parsing

So far, so good. Parsing a user for a hostname is also rather straightforward:
We just consume characters until we hit upon an illegal char or `@`. Whenever
we hit `@`, we start with the really ugly part here: The host parsing.

I decided to implement the host parsing as complex as I possibly could. I'd like
to do it in an NFA-style, but consume and use values as we go along. As we can
both have IP4, IP6 and hostnames available, this is a bit messy. To recap, the
relevant ABNF part is the following piece:

```abnf
host       =  hostname / hostaddr
hostname   =  shortname *( "." shortname )
shortname  =  ( letter / digit ) [ *( letter / digit / "-" )
                                   ( letter / digit ) ]
hostaddr   =  ip4addr / ip6addr
ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
```

In addition, we accept the `::` shorthand form, and constrain the IP4/IP6
addresses to only legal addresses while lexing. See "Erling's Differences from
RFC 2812" for more information.

In general, the `parse_host` parsing has turned into a really ugly piece of
code, but the concept is not terribly difficult: We "emulate" three parsers in a
single function, and remove possible options once we see that it's impossible to
parse it. We do so by having a *"state"* parameter and an *"ok"* parameter for
each legal output result (hostname/IP4/IP6), and pass those values to an update
function, along with the value in. We get back a new "state" and "ok" parameter,
depending on the logic and values passed in.

In all cases, if "ok" is `false`, the update function short-circuits and returns
`{nil, false}` *in general*. The only oddity here is when we have a possible IP4
address inside the IP6 address, where we "reaccept" the ip4 address. The
following snippet shows what's happening whenever we hit a `:` in the name:

```erl
reaccept_ip4_addr([X, 0, 0, 0, 0, 0])
  when X =:= 0; X =:= 16#FFFF ->
    {[0], true};
reaccept_ip4_addr(_) ->
    {nil, false}.
```

Not too much magic, but there's certainly a huge amount of context you have to
add in whenever playing around with that specific part of the code.
