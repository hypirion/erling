# Erling's Differences from RFC 2812

Erling is supposed to implement RFC 2812 completely. Some erratas, ambiguities
and other problems do exist, however, and Erling tries to take those into
account in the most sensible manner possible. What follows are known differences
between Erling and RFC 2812.

## Errata Fixes

### Shortname

The definition of `shortname` in Section 2.3.1, is defined as:

```abnf
shortname  =  ( letter / digit ) *( letter / digit / "-" )
                *( letter / digit )
                  ; as specified in RFC 1123 [HNAME]
```

In Erling, the definition of `shortname` is defined as follows:

```abnf
shortname  =  ( letter / digit ) [ *( letter / digit / "-" )
                letter / digit ]
```

This takes into account errata #991.

## Ambiguities

### IP4 and Hostname

According to the ABNF grammar defined in RFC 2812, Section 2.3.1, legal IP4
addresses will always be legal hostnames. Whenever one can return either a
hostname or an IP4 address, an IP4 address will be returned.

## Constraints

### IP4 Address Names

IP4 address names are more restricted than how they are specified in RFC 2812.
In Section 2.3.1, `ip4addr` is defined as

```abnf
ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
```

In Erling, the groupings of up to three digits must, when concatenated, have a
value lower or equal to `255`.

Note that whereas `256.103.9.155` is not a legal IP4 address in Erling, it is a
legal hostname. Consequently, if one can receive either an IP4 address or a
servername, one can receive `256.103.9.155`, but it will return as a servername,
not as an IP4 address. For more information, see *Ambiguities* -> *IP4 and
Hostname*.

## Major Differences

### IP6 Address Names

Erling implements IP6 addresses such that they are both more constrained and
more liberal than RFC 2812. The constraints prevents one from attempting to
connect to a clearly erroneous address, and the liberal parsing accepts
addresses represented as specified in RFC 4291 *and* as specified in RFC 2812,
sans obvious erroneous ones.

In Erling, an IP6 address **cannot** contain a hexdigit group which has a value
higher than `FFFF` in hexadecimal, or 65535 in decimal. An IP6 address *may*
have an arbitrary amount of leading zeroes in a hexdigit group. For example
would the hexdigit group `0000ffff` be legal in Erling and RFC 2812, although it
is not a legal hexdigit group according to RFC 4291. The hexdigit group `C0FFEE`
is a legal hexdigit group in RFC 2812, but is not legal in Erling or in RFC
4291.

Erling allows the use of `::`-compression. Consequently, Erling implements a
superset of RFC 4921 for printing.

When Erling prints IP6 addresses, Erling will always print them in a RFC
2812-compliant way. Erling uses the RFC 5952 as guidance to print sensible
strings: All hexadecimal digits will be printed in lowercase, and as compressed
as possible. `::`-compression will not happen.

For example, the IP6 address `1050::5:0600:300C:326B` would be printed as
`1050:0:0:0:5:600:300c:326b`, **not** as
`1050:0000:0000:0000:0005:0600:300c:326b` and **not** as
`1050:0000:0000:0000:0005:0600:300C:326B`.

## References

* [RFC 2812 - Internet Relay Chat: Client Protocol][rfc2812]
* [RFC 4291 - IP Version 6 Addressing Architecture][rfc4291]
* [RFC 5952 - A Recommendation for IPv6 Address Text Representation][rfc5952]

[rfc2812]: http://tools.ietf.org/html/rfc2812
[rfc4291]: http://tools.ietf.org/html/rfc4291
[rfc5952]: http://tools.ietf.org/html/rfc5952
