iso8583
=======

ISO-8583 parser/generator library in Erlang.

This document assumes that the reader has some basic knowledge of [ISO-8583 encoding rules](https://en.wikipedia.org/wiki/ISO_8583/).


Overview
--------

This is an ISO-8583 parsing library, implementing v87 dialect used by [ACI BASE24](https://en.wikipedia.org/wiki/BASE24/) Switch in Host Interface. Implementation is parametrization-driven, and other dialects can be easily added.

Library is a module of an ISO-8583 Adapter, that handles connection to ACI BASE24 Switch, and for each of the incomming message :
- parses message to the map of fields using iso8583 lib
- converts map to JSON
- sends JSON message to RabbitMQ for further processing in the Back-End

Asynchronous responses are handled in a similar way:
- response is read from RabbitMQ Queue
- JSON content is transformed to a map
- binary ISO-8583 message is built from the map using iso8583 lib
- message is sent via ISO-8583 Adatpter to BASE24 Switch

Library provides API for parsing/building binary messages and manipulating individual fields held in a map representing decomposed message.

Base principles
---------------
During parsing, binary ISO-8583 message is decomposed into `#iso_msg{}` structure. This structure holds a map of fields extracted from original message. Parsing is driven by BIC-ISO field definitions, specified in `iso_definitions.erl`. Fields can have 'variant' definitions, depending on the message 'product' code (like `pos`, `atm`, `fhm` and other).

The map of parsed fields is basically `ID -> #iso_field{}` mapping.
`#iso_field{}` holds field value as a binary, and a few other items required for further field processing. 

Some fields have internal sub-structure imposed by BIC-ISO definitions, and this sub-structure can differ based on product code, similar to 'variants'. For example S95 field has different internal encodings for `pos` and `atm` products. This is also defined in `iso_definitions.erl`. However, our experience is that ISO-8583 Adapter should do as little as possible, deferring unnecessary processing. To achieve this, parser takes pragmatic approach and does not undertake sub-structure parsing. Instead, all relevant `#iso_field{}` is equipped with decoder/encoder funs, that can be called by the client if field sub-structure processing is really required.

Sub-structures are decoded/encoded into/from maps of items. Keys in these maps are items tags defined in decomposition definitions in `iso_definitions.erl`.

Binary message building is based on the same idea, just works in the opposite direction.

Tokens
------
BIC-ISO Tokens are supported and defined in `iso_definitions.erl`. Only subset of the full Token Manual definitions is provided in the current version (B2-B4 EMV Tokens are included). `#iso_msg{}` keeps separate maps for fields and for tokens. By convention, tokens for `pos` product go to P63 and for `atm` to S126 field, respectively.

Structures
----------
API uses the following main structures:
`#iso_field{}` holds data of an individual ISO field instance. It has the `id`, `tag`, raw `value` and optional `decoder`/`encoder`, if sub-structure is present.
`#iso_token{}` holds data of individual ISO token instance. It has the `tag`, raw `value` and `decoder`/`encoder`.
`#iso_msg{}` represents parsed ISO message or object prepared for building binary ISO message. It has MTI, product code, `fields`, `tokens` and original binary message.

Basic Usage
-----------
At erl prompt:

```
1> MSG = <<"ISO0260000700200B23C84A128E1801A0000000000000038000000000000002400021311065323108212071702132007021307100606465556364716322785013595=616200722100112233400122614647926142553        000000000263079BAR GOWOREK S.C. STANKWARSZAWA        PL027000000000263079            985016ESRVPRO100000000019BANK    00000000000384& 0000700384! C400012 000000000030! 0400020                   Y ! B200158 7BF900008000000000002711FE3959A251910000000024000000000000000040001161698517021300F1B7C18500321F430071A020000000504B4F00071044009911B92EB7DF4B1D3500000B000000! B300080 C20062513974E06040300000000000000000001F0000000200000000000000000000000000000000! B400020 071800021F0000    0 ! 2000022 O                     020231082              009059000000012VD          ">>.
```

Parsing:

```
2> {ok,PARSED} = iso_parser:parse_message(MSG).
```

By default, library does lots of debug logs. Logging is enabled by {d,debug} entry in rebar.config.

Now, you can examine fields and tokens, but first load record definitions to the shell:

```
3>rr("include/iso_defs.hrl").
4> #iso_msg{fields=FLDS,tokens=TKNS} = PARSED.
5> FLDS.
6> TKNS.
```

Getting individual fields (field #7 is called Transmission Date Time):
```
{ok,#iso_field{value=DT}=P07}=iso_utils:get_field(7,PARSED).
DT.
```

Getting value of individual fields (field #37 is called RRN):

```
{ok,RRN} = iso_utils:get_field_value(37,PARSED).
RRN.
```

Setting some field's value and building binary message:
```
RRN1 = iso_utils:format_integer(133,12).
PATCHED = iso_utils:set_field_value(37,RRN1,PARSED).
CFG = iso_definitions:get_parser_config(biciso).
MSG1 = iso_parser:build_message(PATCHED,CFG).

```
Now we can confirm that the new message has proper value for RRN:
```
{ok,PARSED1} = iso_parser:parse_message(MSG1).
{ok,RRN1X} = iso_utils:get_field_value(37,PARSED1).
RRN1X.
```
And finally, let's decompose P43 (Merchant Name Address): 
```
{ok,P43} = iso_utils:get_field(43,PARSED1).
ITEMS = iso_parser:field_decode_items(P43).
ITEMS.
```

Build
-----
    $ rebar3 compile

Credits
-------
Artur Stefanowicz, artur.stefanowicz7@gmail.com

Licence
-------
Apache 2.0
