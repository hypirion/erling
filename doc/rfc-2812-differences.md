# Erling's Differences from RFC 2812

Erling is supposed to implement RFC 2812 straight ahead. Some erratas do exist,
however, and Erling tries to take those into account.

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
