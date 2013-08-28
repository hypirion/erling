# Internal Explanation of `erling_irc_conv.erl`

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

