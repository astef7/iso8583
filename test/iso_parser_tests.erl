%%==============================================================================
%% Copyright 2020, Artur Stefanowicz <artur.stefanowicz7@gmail.com>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%==============================================================================
%% ISO8583 / Testing utilities
%%------------------------------------------------------------------------------
%% 2018-2020
%%==============================================================================
-module(iso_parser_tests).

-include("iso_defs.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% ---------------------------------------------------------------------------------------------
%%                                   TEST DATA
%% ---------------------------------------------------------------------------------------------

get_mti200(pos) ->
    <<"ISO0260000100200B238800128A18010000000000000010401000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000033UL. WOLSKA 33            00000000404& 0000200404! BP00382 22000000000000000                                                            853690                                                                                                                                 K                                                                                                                              I                                          ">>;

get_mti200(atm) ->
    <<"ISO0160000100200B238800128A18010000000000000010401000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000033UL. WOLSKA 33            00000000404& 0000200404! BP00382 22000000000000000                                                            853690                                                                                                                                 K                                                                                                                              I                                          ">>.

get_mti200_emv() ->
    <<"ISO0260000700200B23C84A128E1801A0000000000000038000000000000002400021311065323108212071702132007021307100606465556364716322785013595=616200722100112233400122614647926142553        000000000263079BAR GOWOREK S.C. STANKWARSZAWA        PL027000000000263079            985016ESRVPRO100000000019BANK    00000000000384& 0000700384! C400012 000000000030! 0400020                   Y ! B200158 7BF900008000000000002711FE3959A251910000000024000000000000000040001161698517021300F1B7C18500321F430071A020000000504B4F00071044009911B92EB7DF4B1D3500000B000000! B300080 C20062513974E06040300000000000000000001F0000000200000000000000000000000000000000! B400020 071800021F0000    0 ! 2000022 O                     020231082              009059000000012VD          ">>.

get_mti210(pos)->
    <<"ISO0260000100210B23880012AA18010000000000000010401000000000000200009111352460004211552460911091109102000000304716322785013595=616491270177700000000970200S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCER1+000033UL. WOLSKA 33            00000000404& 0000200404! BP00382 22000000000000000                                                            853690                                                                                                                                 K                                                                                                                              I                                          ">>;

get_mti210(atm)->
    <<"ISO0160000100210B23880012AA18010000000000000010401000000000000200009111352460004211552460911091109102000000304716322785013595=616491270177700000000970200S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCER1+000033UL. WOLSKA 33            00000000404& 0000200404! BP00382 22000000000000000                                                            853690                                                                                                                                 K                                                                                                                              I                                          ">>.

get_echo()->
    %% S70 = 301 to ECHO
    <<"ISO00600006008008220000000010000040000000000000008191055240020690176011002212N009852301">>.

get_cut_off()->
    %% S70 = 201 to CUT-OFF
    <<"ISO006000060080082220000000100000400000000000000122408550000000109250176011002212N009852201">>.

get_sign_on()->
    %% S70 = 001 to SIGN-ON
    <<"ISO00600006008008220000000010000040000000000000012240855000000010176011002212N009852001">>.

get_sign_off()->
    %% S70 = 002 to SIGN-OFF
    <<"ISO00600006008008220000000010000040000000000000012240855000000010176011002212N009852002">>.

get_mti420(atm)->
    <<"ISO0160000170420B23A80012EA080100000004000000004010000000000050000040212593801486714591404020402040209102000000304716322785013595=61649127010005047        M0000821S1PO2032        04027 2032N5886C      POZNAN       POZPL985012BANKCXY1+00002005047        0402145914040402          542& 0000300542! BP00382 22000000000000000100                                                         897458                   48502608345              11999993                 247B3BC82C5477F0B5252A08AFDF902BIND.I                       OTT                                               ROR45102011112222333344445535                               00000014158805474B                                          ! 0800128 03O897458                   48502111145    TT14529834101                                                                        ">>.

%% ---------------- Testy Parse/Gen -----------------------------------------------------------------

msg200_pos_test_() ->
    MSG = get_mti200(pos),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    %% ?debugFmt("msg200_pos_test_ : prod=~p~n",[PROD]),
    
    [
     ?_assertEqual(pos, PROD),
     ?_assertMatch(#{4 := #iso_field{value= <<"000000002000">>}},FLDS_PARSED),
     ?_assertMatch(#{12 := #iso_field{value= <<"155246">>}},FLDS_PARSED),
     ?_assertMatch(#{49 := #iso_field{value= <<"985">>}},FLDS_PARSED)
    ].

msg200_pos_no_sbitmap_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG = get_mti200(pos),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=ISO_MSG} = iso_parser:parse_message(MSG),

    FLDS1 = maps:without([120,123,124,125,126],FLDS_PARSED),
    MSG1 = iso_parser:build_message(ISO_MSG#iso_msg{fields=FLDS1},CFG),
    {ok,#iso_msg{org=_MSG_ORG1,
		 fields=FLDS_PARSED1,
		 mti=_MTI1,
		 prod=PROD1}=_ISO_MSG1} = iso_parser:parse_message(MSG1),

    ?debugFmt("msg200_pos_no_sbitmap_test_ : FLDS_PARSED1=~p~n",[FLDS_PARSED1]),

    [
     ?_assertEqual(<<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>, MSG1)
    ].

msg200_show_bitmap_test_() ->
    MSG = get_mti200(pos),
    PBTM = iso_parser:get_bitmap(primary,MSG),
    ?debugFmt("Bitmap=~p~n",[iso_parser:show_bitmap(PBTM)]),
    [
     ?_assertEqual(<<"B238800128A18010">>,PBTM)
    ].

msg200_pos_emv_test_() ->
    MSG = get_mti200_emv(),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 tokens=TOKENS,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("msg200_pos_emv_test_ : tokens=~p~n",[TOKENS]),
    
    [
     ?_assertEqual(pos, PROD),
     ?_assertMatch(#{4 := #iso_field{value= <<"000000002400">>}},FLDS_PARSED),
     ?_assertMatch(#{12 := #iso_field{value= <<"120717">>}},FLDS_PARSED),
     ?_assertMatch(#{49 := #iso_field{value= <<"985">>}},FLDS_PARSED)
    ].

msg210_pos_test_() ->
    MSG = get_mti210(pos),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    %% ?debugFmt("msg210_pos_test_ : prod=~p~n",[PROD]),
    
    [
     ?_assertEqual(pos, PROD),
     ?_assertMatch(#{4 := #iso_field{value= <<"000000002000">>}},FLDS_PARSED),
     ?_assertMatch(#{12 := #iso_field{value= <<"155246">>}},FLDS_PARSED),
     ?_assertMatch(#{39 := #iso_field{value= <<"00">>}},FLDS_PARSED),
     ?_assertMatch(#{49 := #iso_field{value= <<"985">>}},FLDS_PARSED)
    ].

msg200_atm_test_() ->
    MSG = get_mti200(atm),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    %% ?debugFmt("msg200_atm_test_ : prod=~p~n",[PROD]),
    
    [
     ?_assertEqual(atm, PROD),
     ?_assertMatch(#{4 := #iso_field{value= <<"000000002000">>}},FLDS_PARSED),
     ?_assertMatch(#{12 := #iso_field{value= <<"155246">>}},FLDS_PARSED),
     ?_assertMatch(#{49 := #iso_field{value= <<"985">>}},FLDS_PARSED)
    ].

msg210_atm_test_() ->
    MSG = get_mti210(atm),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    %% ?debugFmt("msg210_atm_test_ : prod=~p~n",[PROD]),
    
    [
     ?_assertEqual(atm, PROD),
     ?_assertMatch(#{4 := #iso_field{value= <<"000000002000">>}},FLDS_PARSED),
     ?_assertMatch(#{12 := #iso_field{value= <<"155246">>}},FLDS_PARSED),
     ?_assertMatch(#{39 := #iso_field{value= <<"00">>}},FLDS_PARSED),
     ?_assertMatch(#{49 := #iso_field{value= <<"985">>}},FLDS_PARSED)
    ].

msg420_atm_test_() ->
    MSG = get_mti420(atm),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("msg420_pos_test_ : FIELDS=~p~n",[FLDS_PARSED]),

    #{4 := P4, 12 := P12, 49 := P49, 90 := S90} = FLDS_PARSED, %% Uwaga: w tym komunikacie nie ma S95 ...
    
    [
     ?_assertEqual(atm, PROD),
     ?_assertEqual(<<"000000050000">>,P4#iso_field.value),
     ?_assertEqual(<<"145914">>,P12#iso_field.value),
     ?_assertEqual(<<"985">>,P49#iso_field.value),
     ?_assertEqual(<<"02005047        0402145914040402          ">>,S90#iso_field.value)
    ].

msg800_signon_test_() ->
    MSG = get_sign_on(),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("sign_on_test_ : FIELDS=~p~n",[FLDS_PARSED]),

    #{70 := S70} = FLDS_PARSED,
    
    [
     ?_assertEqual(<<"001">>,S70#iso_field.value)
    ].

msg800_signoff_test_() ->
    MSG = get_sign_off(),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("sign_on_test_ : FIELDS=~p~n",[FLDS_PARSED]),

    #{70 := S70} = FLDS_PARSED,
    
    [
     ?_assertEqual(<<"002">>,S70#iso_field.value)
    ].

msg800_cutoff_test_() ->
    MSG = get_cut_off(),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("sign_on_test_ : FIELDS=~p~n",[FLDS_PARSED]),

    #{70 := S70} = FLDS_PARSED,
    
    [
     ?_assertEqual(<<"201">>,S70#iso_field.value)
    ].

msg800_echo_test_() ->
    MSG = get_echo(),
    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("sign_on_test_ : FIELDS=~p~n",[FLDS_PARSED]),

    #{70 := S70} = FLDS_PARSED,
    
    [
     ?_assertEqual(<<"301">>,S70#iso_field.value)
    ].

sub_field_decoder_S90_test_()->
    CFG = iso_definitions:get_parser_config(biciso),
    {_TAG,_EXC,_LEN,_DTYPE,DECODER_DECOMP,_} = iso_utils:get_field_defs(90,pos,CFG),
    ?debugFmt("Decomposition for 90=~p~n",[DECODER_DECOMP]),
    DECODER = case DECODER_DECOMP of
		  {decoder,DEC} -> DEC;
		  {_,DECOMP1} ->
		      iso_parser:sub_field_decoder_gen(DECOMP1)
	      end,
    S90 = <<"02005047        0402145914040402          ">>,
    MX = DECODER(S90), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for 90=~p~n",[MX]),
    [
     ?_assertMatch(#{"ORG-MTI" := <<"0200">>},MX),
     ?_assertMatch(#{"ORG-TX-DATE" := <<"0402">>},MX)
    ].

sub_field_encoder_S90_test_()->
    CFG = iso_definitions:get_parser_config(biciso),
    {_TAG,_EXC,_LEN,DTYPE,DECODER_DECOMP,ENCODER_DECOMP} = iso_utils:get_field_defs(90,pos,CFG),
    ?debugFmt("Decomposition for 90=~p~n",[DECODER_DECOMP]),
    DECODER = case DECODER_DECOMP of
	    {decoder,DEC} -> DEC;
	    {_,DECOMP} ->
		iso_parser:sub_field_decoder_gen(DECOMP)
	end,
    S90 = <<"02005047        0402145914040402          ">>,
    MX = DECODER(S90), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode

    ENCODER = case ENCODER_DECOMP of %% obecnie nie ma inline-encoderow...
		  {decomposition,DECOMP2} ->
		      iso_parser:sub_field_encoder_gen(DECOMP2,DTYPE);
		  {_,_} ->
		      error(no_inline_encoders)
	      end,
    S90X = ENCODER(MX), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition: original S90=~p~n",[S90]),
    ?debugFmt("Decomposition:   MAP for 90=~p~n",[MX]),
    ?debugFmt("Decomposition:      new S90=~p~n",[S90X]),
    [
     ?_assertEqual(S90,S90X)
    ].

sub_field_decoder_P43_test_()->
    CFG = iso_definitions:get_parser_config(biciso),
    {_TAG,_EXC,_LEN,_DTYPE,DECODER_DECOMP,_} = iso_utils:get_field_defs(43,pos,CFG),
    ?debugFmt("Decomposition for 43=~p~n",[DECODER_DECOMP]),
    DECODER = case DECODER_DECOMP of
		  {decoder,DEC} -> DEC;
		  {_,DECOMP1} ->
		      iso_parser:sub_field_decoder_gen(DECOMP1)
	      end,
    P43 = <<"01026 8803N5870W B/W  WARSZAWA     WARPL">>,
    MX = DECODER(P43), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for 43=~p~n",[MX]),
    [
     ?_assertMatch(#{"TRM-OWNER" := <<"01026 8803N5870W B/W  ">>},MX),
     ?_assertMatch(#{"TRM-CITY" := <<"WARSZAWA     ">>},MX),
     ?_assertMatch(#{"TRM-CNTRY" := <<"PL">>},MX)
    ].

sub_field_decoder_P35_test_()->
    CFG = iso_definitions:get_parser_config(biciso),
    {_TAG,_EXC,_LEN,_DTYPE,DECODER_DECOMP,_} = iso_utils:get_field_defs(35,pos,CFG),
    ?debugFmt("Decomposition for 35=~p~n",[DECODER_DECOMP]),
    DECODER = case DECODER_DECOMP of
		  {decoder,DEC} -> DEC;
		  {_,DECOMP1} ->
		      iso_parser:sub_field_decoder_gen(DECOMP1)
	      end,

    P35 = <<"4716322785013595=6164912701777">>,
    MX = DECODER(P35), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for P35=~p~n",[MX]),
    
    [
     ?_assertMatch(#{"PAN" := <<"4716322785013595">>},MX),
     ?_assertMatch(#{"EXPDT" := <<"4912">>},MX)
    ].

sub_field_encoder_P35_test_()->
    CFG = iso_definitions:get_parser_config(biciso),
    {_TAG,_EXC,_LEN,DTYPE,_,ENCODER_DECOMP} = iso_utils:get_field_defs(35,pos,CFG),
    ?debugFmt("Decomposition for 35=~p~n",[ENCODER_DECOMP]),
    ENCODER = case ENCODER_DECOMP of
		  {encoder,ENC} -> ENC;
		  {_,DECOMP1} ->
		      iso_parser:sub_field_encoder_gen(DECOMP1,DTYPE)
	      end,

    P35V = ENCODER(#{"PAN" => <<"4716322785013595">>, "EXPDT" => <<"4912">>}),
    
    [
     ?_assertMatch(<<"4716322785013595=6164912701777">>,P35V)
    ].

msg200_01_gen_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200(pos),

    {ok,#iso_msg{org=_MSG_ORG,
		 fields=_FLDS_PARSED,
		 mti=_MTI,
		 prod=_PROD}=ISO_MSG0} = iso_parser:parse_message(MSG0),
 
    MSG1 = iso_parser:build_message(ISO_MSG0,CFG),
    ?debugFmt("MSG0=~p~n",[MSG0]),
    ?debugFmt("MSG1=~p~n",[MSG1]),

    [
     ?_assertEqual(MSG0,MSG1)
    ].

msg200_01_token_test_() ->
    MSG = get_mti200(pos),

    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 tokens=TOKENS,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("Fields=~p~n",[FLDS_PARSED]),
    ?debugFmt("Token=~p~n",[TOKENS]),

    {ok,P126} = maps:find(126,FLDS_PARSED),
    ?debugFmt("S126=~p~n",[P126]),
    M=iso_parser:parse_tokens(P126#iso_field.value),
    {ok,T} = maps:find("BP",M),
    [
     ?_assertEqual("BP",T#iso_token.tag)
    ].

msg200_01_parse_gen_token_test() ->
    MSG = get_mti200_emv(),

    {ok,#iso_msg{org=_MSG_ORG,
		 fields=FLDS_PARSED,
		 tokens=TOKENS,
		 mti=_MTI,
		 prod=_PROD}=_ISO_MSG} = iso_parser:parse_message(MSG),

    ?debugFmt("parse_gen_token/Fields=~p~n",[FLDS_PARSED]),
    ?debugFmt("parse_gen_token/Token=~p~n",[TOKENS]),

    {ok,P63} = maps:find(63,FLDS_PARSED),
    ?debugFmt("parse_gen_token/S126=~p~n",[P63]),
    TKN_MAP=iso_parser:parse_tokens(P63#iso_field.value),
    {ok,B2}=maps:find("B2",TKN_MAP),
    B2_MAP=iso_parser:token_decode_items(B2),
    ?debugFmt("parse_gen_token/B2_MAP=~p~n",[B2_MAP]),
    B2_MAP1=B2_MAP#{"CRYPTO-INFO-DATA" => <<"99">>},
    {_,B2X} = iso_parser:token_encode_items("B2",B2_MAP1),
    ?debugFmt("parse_gen_token/B2=~p~n",[B2]),
    ?debugFmt("parse_gen_token/B2X=~p~n",[B2X]),

    B3_MAP=iso_parser:token_decode_items(B2X),
    #{"CRYPTO-INFO-DATA" := X}=B3_MAP,
    [
     ?_assertEqual(<<"99">>,X)
    ].

%% No full-parsing: just header and patch some fields...
msg200_01_parse_hdr_and_patch_test_() ->
    MSG0 = get_mti200_emv(),

    {hdr,MTI0,_HPRD,_PBTMP,_SBTMP} = iso_parser:parse_header(MSG0),

    ?debugFmt("parse_hdr_nad_patch/MTI=~p~n",[MTI0]),
    
    % RRN Offset in this msg type is 152...
    RRN0 = iso_utils:get_rrn({offset,152},MSG0),
    ?debugFmt("parse_hdr_nad_patch/RRN0=~p~n",[RRN0]),

    %% just example: setting new RRN1=103
    {RRN1,MSG1} = iso_utils:set_rrn({int,103},{offset,152},MSG0),
    ?debugFmt("parse_hdr_nad_patch/new RRN1=~p~n",[RRN1]),

    ?debugFmt("parse_hdr_nad_patch/MSG0=~p~n",[MSG0]),
    ?debugFmt("parse_hdr_nad_patch/MSG1=~p~n",[MSG1]),

    {ok,#iso_msg{fields=FLDS_PARSED1}} = iso_parser:parse_message(MSG1),
    {ok,#iso_field{value=RRN1X}} = maps:find(37,FLDS_PARSED1),

    %% and for comparision...
    {ok,#iso_msg{fields=FLDS_PARSED0}} = iso_parser:parse_message(MSG0),
    
    ?debugFmt("parse_hdr_nad_patch/Fields0=~p~n",[FLDS_PARSED0]),
    ?debugFmt("parse_hdr_nad_patch/Fields1=~p~n",[FLDS_PARSED1]),
    
    [
     ?_assertEqual(RRN1,RRN1X)
    ].

%% Full-parsing + field modification
msg200_01_parse_and_modify_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    {hdr,MTI0,_HPRD,_PBTMP,_SBTMP} = iso_parser:parse_header(MSG0),
    {ok,#iso_msg{fields=FLDS_PARSED0}=ISO_MSG0} = iso_parser:parse_message(MSG0),

    ?debugFmt("parse_and_modify/MTI=~p~n",[MTI0]),
    
    {ok,RRN0} = iso_utils:get_field_value(37,ISO_MSG0),
    ?debugFmt("parse_and_modify/RRN0=~p~n",[RRN0]),

    %% just example: setting new RRN1=133
    RRN1 = iso_utils:format_integer(133,12),
    ISO_MSG1 = iso_utils:set_field_value(37,RRN1,ISO_MSG0),
    MSG1 = iso_parser:build_message(ISO_MSG1,CFG),

    ?debugFmt("parse_and_modify/MSG0=~p~n",[MSG0]),
    ?debugFmt("parse_and_modify/MSG1=~p~n",[MSG1]),

    {ok,#iso_msg{fields=FLDS_PARSED1}=ISO_MSG1_1} = iso_parser:parse_message(MSG1),
    {ok,RRN1X} = iso_utils:get_field_value(37,ISO_MSG1_1),

    
    ?debugFmt("parse_and_modify/Fields0=~p~n",[FLDS_PARSED0]),
    ?debugFmt("parse_and_modify/Fields1=~p~n",[FLDS_PARSED1]),
    
    [
     ?_assertEqual(RRN1,RRN1X)
    ].


%% Full-parsing + field modification / over-length
msg200_02_parse_and_modify_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    {hdr,MTI0,_HPRD,_PBTMP,_SBTMP} = iso_parser:parse_header(MSG0),
    {ok,#iso_msg{fields=FLDS_PARSED0}=ISO_MSG0} = iso_parser:parse_message(MSG0),

    ?debugFmt("parse_and_modify/MTI=~p~n",[MTI0]),
    
    {ok,RRN0} = iso_utils:get_field_value(37,ISO_MSG0),
    ?debugFmt("parse_and_modify/RRN0=~p~n",[RRN0]),

    RRN1 = <<"000000000999xxx">>, %% over-length...    
    [
     ?_assertError(maxlen_exceeded,iso_utils:set_field_value(37,RRN1,ISO_MSG0))
    ].


token_B2_decoder_test_()->
    B2= <<"7BF900008000000000002711FE3959A251910000000024000000000000000040001161698517021300F1B7C18500321F430071A020000000504B4F00071044009911B92EB7DF4B1D3500000B000000">>,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B2",decomposition=DECOMP}} = maps:find("B2",TKNCFG),
    ?debugFmt("Decomposition for TKN-B2=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B2), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B2=~p~n",[MX]),
    [
     ?_assertMatch(#{"USER-FLD1" := <<"0000">>},MX),
     ?_assertMatch(#{"CRYPTO-INFO-DATA" := <<"80">>},MX),
     ?_assertMatch(#{"AMT-AUTH" := <<"000000002400">>},MX),
     ?_assertMatch(#{"TVR" := <<"0000000000">>},MX),
     ?_assertMatch(#{"ARQC" := <<"2711FE3959A25191">>},MX)
    ].

token_B3_decoder_test_()->
    B3= <<"C20062513974E06040300000000000000000001F0000000200000000000000000000000000000000">>,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B3",decomposition=DECOMP}} = maps:find("B3",TKNCFG),
    ?debugFmt("Decomposition for TKN-B3=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B3), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B3=~p~n",[MX]),
    [
     ?_assertMatch(#{"TERM-SERL-NUM" := <<"62513974">>},MX),
     ?_assertMatch(#{"EMV-TERM-CAP" := <<"E0604030">>},MX),
     ?_assertMatch(#{"CVM-RSLTS" := <<"1F0000">>},MX)
    ].

token_B4_decoder_test_()->
    B4= <<"071800021F0000    0 ">>,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B4",decomposition=DECOMP}} = maps:find("B4",TKNCFG),
    ?debugFmt("Decomposition for TKN-B4=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B4), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B4=~p~n",[MX]),
    [
     ?_assertMatch(#{"PT-SRV-ENTRY-MDE" := <<"071">>},MX),
     ?_assertMatch(#{"TERM-ENTRY-CAP" := <<"8">>},MX),
     ?_assertMatch(#{"APPL-PAN-SEQ-NUM" := <<"02">>},MX),
     ?_assertMatch(#{"CVM-RSLTS" := <<"1F0000">>},MX)
    ].

token_B2_encoder_test_()->
    REFERENCE= <<"! B200158 7BF900008000000000002711FE3959A251910000000024000000000000000040001161698517021300F1B7C18500321F430071A020000000504B4F00071044009911B92EB7DF4B1D3500000B000000">>,
    <<_Pfx:10/bytes,B2/bitstring>> = REFERENCE,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B2",decomposition=DECOMP}} = maps:find("B2",TKNCFG),
    ?debugFmt("Decomposition for TKN-B2=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B2), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B2=~p~n",[MX]),

    {EXT_VAL,#iso_token{tag="B2",value=_VAL}} = iso_parser:token_encode_items("B2",MX),
    ?debugFmt("Builder for TKN-B2=~p~n",[EXT_VAL]),

    [
     ?_assertEqual(REFERENCE,EXT_VAL)
    ].

token_B3_encoder_test_()->
    REFERENCE= <<"! B300080 C20062513974E06040300000000000000000001F0000000200000000000000000000000000000000">>,
    <<_Pfx:10/bytes,B3/bitstring>> = REFERENCE,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B3",decomposition=DECOMP}} = maps:find("B3",TKNCFG),
    ?debugFmt("Decomposition for TKN-B3=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B3), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B3=~p~n",[MX]),

    {EXT_VAL,#iso_token{tag="B3",value=_VAL}} = iso_parser:token_encode_items("B3",MX),
    ?debugFmt("Builder for TKN-B3=~p~n",[EXT_VAL]),

    [
     ?_assertEqual(REFERENCE,EXT_VAL)
    ].

token_B4_encoder_test_()->
    REFERENCE= <<"! B400020 071800021F0000    0 ">>,
    <<_Pfx:10/bytes,B4/bitstring>> = REFERENCE,
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag="B4",decomposition=DECOMP}} = maps:find("B4",TKNCFG),
    ?debugFmt("Decomposition for TKN-B4=~p~n",[DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    MX = DEC(B4), % 'raw' decoder/encoder test. When parsing message, use sub_field_decode/encode
    ?debugFmt("Decomposition MAP for TKN-B4=~p~n",[MX]),

    {EXT_VAL,#iso_token{tag="B4",value=_VAL}} = iso_parser:token_encode_items("B4",MX),
    ?debugFmt("Builder for TKN-B4=~p~n",[EXT_VAL]),

    [
     ?_assertEqual(REFERENCE,EXT_VAL)
    ].

token_external_encoder_test_()->
    TKN126= <<"& 0000700384! C400012 000000000030! 0400020                   Y ! B200158 7BF900008000000000002711FE3959A251910000000024000000000000000040001161698517021300F1B7C18500321F430071A020000000504B4F00071044009911B92EB7DF4B1D3500000B000000! B300080 C20062513974E06040300000000000000000001F0000000200000000000000000000000000000000! B400020 071800021F0000    0 ! 2000022 O                     020231082              009059000000012VD          ">>,
    MAP0=iso_parser:parse_tokens(TKN126),
    ?debugFmt("Tokens MAP for S126 EMV=~p~n",[MAP0]),
    
    EXT=iso_parser:encode_tokens(MAP0),
    MAP1=iso_parser:parse_tokens(EXT),

    KEYS0 = maps:keys(MAP0),
    KEYS1 = maps:keys(MAP1),
    ?debugFmt("Tokens EXT generated for S126 EMV=~p~n",[EXT]),

    [
     ?_assertEqual(binary:part(TKN126,0,12),binary:part(EXT,0,12)),
     ?_assert((maps:without(KEYS0,MAP1) =:= #{}) and (maps:without(KEYS1,MAP0) =:= #{}))
    ].

paraller_parsing_test_()->
    MSG = get_mti200_emv(),
    NTESTS = 100,
    PROCS = 100,
    {spawn, 
     {setup,
      fun()-> %% SETUP
	      T1 = erlang:monotonic_time(),
	      {T1,MSG}
      end,
      fun({T1,_})-> %% CLEANUP
	      T2 = erlang:monotonic_time(),
	      TD = erlang:convert_time_unit(T2-T1,native,micro_seconds),
	      ?debugFmt("paraller_parsing_test, TD=~pms~n",[TD/math:pow(10,3)]),
	      ?debugFmt("paraller_parsing_test, Per TX=~pms~n",[TD/(math:pow(10,3)*NTESTS)])
      end,
      fun({_,MSG1})-> %% Instantiator
	      F = fun(_N,ACC)->
			  [?_test(
			      begin
				  iso_parser:parse_message(MSG1)
				  %% ?debugFmt("paraller_test, N=~p finished~n",[N])
			      end) | ACC ]
		  end,
	      L = lists:foldl(F,[],lists:seq(1,NTESTS)),
	      {inparallel,PROCS,L}
      end
     }}.

%% generate_fixed dla dlugosci > MaxLen
%% to samo dla var
%% iso_parser:get_header_and_bitmap
%% #275 jest bez sensu

msg200_02_field_decode_items_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    {ok,#iso_msg{fields=FLDS_PARSED0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,P43} = maps:find(43,FLDS_PARSED0),
    P43_ITEMS = iso_parser:field_decode_items(P43),
    ?debugFmt("Decode items for P43=~p~n",[P43_ITEMS]),
    [
     ?_assertEqual({ok,<<"BAR GOWOREK S.C. STANK">>},maps:find("TRM-OWNER",P43_ITEMS))
    ].

msg200_02_field_encode_items_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    {ok,#iso_msg{fields=FLDS_PARSED0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,P43} = maps:find(43,FLDS_PARSED0),
    ?debugFmt("P43=~p~n",[P43]),
    P43_ITEMS = iso_parser:field_decode_items(P43),
    P43_ITEMS_2 = P43_ITEMS#{"TRM-OWNER" := "New TRM Owner"},
    P43_2=iso_parser:field_encode_items(P43,P43_ITEMS_2),
    [
     ?_assertEqual(P43_2#iso_field.value,<<"New TRM Owner         WARSZAWA        PL">>)
    ].

msg200_02_bitmap_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    PBTMP = iso_parser:get_bitmap(primary,MSG0),
    SBTMP = iso_parser:get_bitmap(secondary,MSG0),
    ?debugFmt("PBTMP=~p~n",[PBTMP]),
    ?debugFmt("SBTMP=~p~n",[SBTMP]),
    [
     ?_assertEqual(<<"B23C84A128E1801A">>,PBTMP),
     ?_assertEqual(<<"0000000000000038">>,SBTMP)
    ].

msg200_02_header_and_bitmap_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = get_mti200_emv(),

    {H,P,S}=HB = iso_parser:get_header_and_bitmap(MSG0),
    ?debugFmt("Header and bitmap=~p~n",[HB]),
    [
     ?_assertEqual(<<"ISO0260000700200">>,H),
     ?_assertEqual(<<"B23C84A128E1801A">>,P),
     ?_assertEqual(<<"0000000000000038">>,S)
    ].

%% No sec-bitmap...
msg200_03_header_and_bitmap_test_() ->
    CFG = iso_definitions:get_parser_config(biciso),
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,

    {H,P,S}=HB = iso_parser:get_header_and_bitmap(MSG0),
    ?debugFmt("Header and bitmap=~p~n",[HB]),
    [
     ?_assertEqual(<<"ISO0260000100200">>,H),
     ?_assertEqual(<<"3238800128A18010">>,P),
     ?_assertEqual(none,S)
    ].

get_mti_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {_,MTI,_}=iso_utils:get_mti(MSG0),
    [
     ?_assertEqual(<<"0200">>,MTI)
    ].

set_mti_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    MSG1 = iso_utils:set_mti(<<"0400">>,MSG0),
    {_,MTI1,_}=iso_utils:get_mti(MSG1),
    [
     ?_assertEqual(<<"0400">>,MTI1)
    ].

get_stan_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,#iso_msg{fields=FLDS0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    ?debugFmt("FLDS for STAN=~p~n",[FLDS0]),
    STAN0=iso_utils:get_stan({offset,60},MSG0),
    [
     ?_assertEqual(<<"000421">>,STAN0)
    ].

set_stan_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,#iso_msg{fields=FLDS0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    {_,MSG1}=iso_utils:set_stan({int,111},{offset,60},MSG0),
    STAN1=iso_utils:get_stan({offset,60},MSG1),
    [
     ?_assertEqual(<<"000111">>,STAN1)
    ].

get_pan_expdt_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,#iso_msg{fields=FLDS0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    ?debugFmt("FLDS for PAN/EXPDT=~p~n",[FLDS0]),
    {PAN,EXPDT}=iso_utils:get_pan_expdt(ISO_MSG0),
    [
     ?_assertEqual(<<"4716322785013595">>,PAN),
     ?_assertEqual(<<"4912">>,EXPDT)
    ].

set_pan_expdt_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,#iso_msg{fields=FLDS0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    ?debugFmt("FLDS for PAN/EXPDT=~p~n",[FLDS0]),
    ISO_MSG1=iso_utils:set_pan_expdt(<<"4400000000000055">>,<<"2112">>,ISO_MSG0),
    {PAN,EXPDT}=iso_utils:get_pan_expdt(ISO_MSG1),
    [
     ?_assertEqual(<<"4400000000000055">>,PAN),
     ?_assertEqual(<<"2112">>,EXPDT)
    ].

set_transmission_dt_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,#iso_msg{fields=FLDS0}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    ?debugFmt("FLDS for Transmission DT=~p~n",[FLDS0]),
    ISO_MSG1=iso_utils:set_transmission_dt(<<"1225213317">>,ISO_MSG0),
    {ok,#iso_field{value=DT}=P07}=iso_utils:get_field(7,ISO_MSG1),
    [
     ?_assertEqual(<<"1225213317">>,DT)
    ].

dump_01_test_()->
    MSG0 = <<"ISO02600001002003238800128A1801001000000000000200009111352460004211552460911091109102000000304716322785013595=61649127017773881        S1XY8803        01026 8803N5870W B/W  WARSZAWA     WARPL044A                       40000061600000000010985012BANKCXY1+000">>,
    {ok,ISO_MSG0} = iso_parser:parse_message(MSG0),
    DMP=iso_utils:dump(ISO_MSG0),
    ?debugFmt("MSG DUMP:~n~s~n",[DMP]),
    [].

decode_S90_01_test_()->
    MSG0 = get_mti420(atm),
    {ok,ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,#iso_field{value=VAL}=S90}=iso_utils:get_field(90,ISO_MSG0),
    {OrgMTI,OrgSEQ,OrgTxDt,OrgTxTm,OrgCapDt}=S90Items=iso_utils:decode_S90(VAL),
    ?debugFmt("S90:~p~n",[S90Items]),
    [
     ?_assertEqual(<<"0200">>,OrgMTI),
     ?_assertEqual(<<"0402">>,OrgCapDt)
    ].

decode_S95_01_test_()->
    MSG0 = get_mti420(atm),
    {ok,#iso_msg{prod=PROD}=ISO_MSG0} = iso_parser:parse_message(MSG0),
    %% however, this is full reversal with no S95. We have to insert this...
    S95= <<"000000003300000000003300000000000000000000">>,
    ISO_MSG1 = iso_utils:set_field_no_fmt(95,S95,ISO_MSG0),
    
    {ok,#iso_field{value=VAL}}=iso_utils:get_field(95,ISO_MSG1),
    {ActTxAmt,StlAmt,TxFee,StlFee}=S95Items=iso_utils:decode_S95(PROD,VAL),
    ?debugFmt("S95:~p~n",[S95Items]),
    [
     ?_assertEqual(<<"000000003300">>,ActTxAmt),
     ?_assertEqual(<<"000000000">>,StlFee)
    ].

decode_S70_01_test_()->
    MSG0 = get_echo(),
    {ok,ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,#iso_field{value=VAL}}=iso_utils:get_field(70,ISO_MSG0),
    S70Literal = iso_utils:decode_S70(VAL),
    [
     ?_assertEqual("ECHO",S70Literal)
    ].

decode_S70_02_test_()->
    MSG0 = get_sign_on(),
    {ok,ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,#iso_field{value=VAL}}=iso_utils:get_field(70,ISO_MSG0),
    S70Literal = iso_utils:decode_S70(VAL),
    [
     ?_assertEqual("SIGN-ON",S70Literal)
    ].

decode_S70_03_test_()->
    MSG0 = get_sign_off(),
    {ok,ISO_MSG0} = iso_parser:parse_message(MSG0),
    {ok,#iso_field{value=VAL}}=iso_utils:get_field(70,ISO_MSG0),
    S70Literal = iso_utils:decode_S70(VAL),
    [
     ?_assertEqual("SIGN-OFF",S70Literal)
    ].
