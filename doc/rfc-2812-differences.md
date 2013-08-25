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
