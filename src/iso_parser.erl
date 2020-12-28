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
%% ISO8583 / BIC ISO Parser
%%------------------------------------------------------------------------------
%% 2018-2020
%%==============================================================================
-module(iso_parser).
-vsn("3.0.11").
-include("iso_defs.hrl").

-export([parse_header/1,parse_message/1, parse_tokens/1, build_message/2]).

-export([field_decode_items/1,field_encode_items/2]).
-export([sub_field_decoder_gen/1,sub_field_encoder_gen/2]).

-export([encode_tokens/1]).
-export([token_decode_items/1,token_encode_items/2]).

-export([get_bitmap/2,get_header_and_bitmap/1,show_bitmap/1]).

-ignore_xref([{?MODULE, get_bitmap, 2}]).
-ignore_xref([{?MODULE, get_header_and_bitmap, 1}]).

-ifdef(debug).
-define(LOG(FMT,VALS),io:format(FMT++"\n",VALS)).
-else.
-define(LOG(FMT,VALS),ok).
-endif.

-define(DIALECT,biciso).
-define(MTI_LEN,4).
-define(BTMP_LEN,16).
-define(HDR_LEN,16).
-define(HDR_PFX_DEFAULT,"ISO026000010").

%%-------------------------------------------------------------------------------------------------
%%---------------------------------- Field parsing ------------------------------------------------
%%-------------------------------------------------------------------------------------------------
-spec parse_fixed(Msg::binary(),Off::integer(),Len::integer())-> 
	  {Fld::binary(),NewOff::integer()}.
parse_fixed(Msg,Off,Len) ->
    PfxLen = Off-1,
    <<_Pfx:PfxLen/bytes,Fld:Len/bytes,_Rest/bitstring>> = Msg,
    {Fld,Off+Len}.

-spec generate_fixed(Val::list(),Len::integer())-> 
	  {Val::binary(),Len::integer()}.
generate_fixed(Val,Len) when is_list(Val)->
    Val1 = case length(Val)>Len of
	       true ->
		   lists:sublist(Val,Len);
	       false ->
		   lists:flatten(string:pad(Val,Len,trailing))
	   end,
    ?LOG("generate_fixed=~p,len=~p",[Val1,Len]),
    {list_to_binary(Val1),Len}.

-spec parse_llvar(Msg::binary(),Off::integer())-> 
	  {Val::binary(),NewOff::integer()}.
parse_llvar(Msg,Off) ->
    parse_var(Msg,Off,2).

-spec parse_lllvar(Msg::binary(),Off::integer())-> 
	  {Val::binary(),NewOff::integer()}.
parse_lllvar(Msg,Off) ->
    parse_var(Msg,Off,3).

-spec parse_var(Msg::binary(),Off::integer(),VarLen::integer())-> 
	  {Val::binary(),NewOff::integer()}.
parse_var(Msg,Off,VarLen) ->
    PfxLen = Off-1,
    <<_Pfx:PfxLen/bytes,Len:VarLen/bytes,Rest/bitstring>> = Msg,
    L=list_to_integer(binary_to_list(Len)),
    <<Fld:L/bytes,_Rest/bitstring>> = Rest,
    ?LOG("VAR[~p], LEN(decoded)=~p, LEN=~p",[Fld,L,Len]),
    {Fld,Off+L+VarLen}.

-spec generate_llvar(Val::list(),MaxLen::integer())-> 
	  {Val::binary(),NewOff::integer()}.
generate_llvar(Val,MaxLen)->
    generate_var(Val,MaxLen,2).

-spec generate_lllvar(Val::list(),MaxLen::integer())-> 
	  {Val::binary(),NewOff::integer()}.
generate_lllvar(Val,MaxLen)->
    generate_var(Val,MaxLen,3).

-spec generate_var(Val::list(),MaxLen::integer(),VarLen::integer())-> 
	  {Val::binary(),NewOff::integer()}.
generate_var(Val,MaxLen,VarLen)->
    Val1 = case (length(Val)>MaxLen) of
	       true -> lists:sublist(Val,MaxLen);
	       false -> Val
	   end,
    LEN = length(Val1),
    case length(integer_to_list(LEN)) =< VarLen of
	true ->
	    Val1LPfx=iso_utils:format_integer(length(Val1),VarLen),
	    ValBin=list_to_binary(Val),
	    {<<Val1LPfx/binary,ValBin/binary>>,LEN+VarLen};
	false ->
	    error(val_too_long)
    end.

%% --------------------------------------------------------------------------------------------
%% ------------------------- Header functions -------------------------------------------------
%% --------------------------------------------------------------------------------------------
-spec parse_header(MSG::binary())-> {hdr,
				     MTI::binary(),
				     HPRDX::iso_prod_type(),
				     PBTMP::binary(),
				     SBTMP::binary()}.
parse_header(MSG) ->
    %% Uwaga: PBTM/SBTM są w HEX dlatego mają po 16b...
    <<_PFX:3/bytes,HPRD:2/bytes,_HREL:2/bytes,_HSTS:3/bytes,_HORG:1/bytes,_HRSP:1/bytes,
      MTI:?MTI_LEN/bytes,PBTMP:?BTMP_LEN/bytes,SBTMP:?BTMP_LEN/bytes,_RST/bitstring>> = MSG,
    ?LOG("PFX=~p,HPRD=~p,MTI=~p,PBTMT=~p",[_PFX,HPRD,MTI,PBTMP]),
    HPRDX = case HPRD of
		<<"00">> ->
		    netmgt;
		<<"01">> ->
		    atm;
		<<"02">> ->
		    pos;
		<<"03">> ->
		    teller;
		<<"08">> ->
		    fhm;
		<<"14">> ->
		    telebanking;
		_ ->
		    unknown
	    end,
    {hdr,MTI,HPRDX,PBTMP,SBTMP}.%przy czym SBTMP ma sens tylko dla P1=1, co jest ustalane na dalszym etapie

-spec generate_header(pos,MTI::string())-> binary().
generate_header(pos,MTI) when is_list(MTI)->
    MTI1 = list_to_binary(MTI),
    <<?HDR_PFX_DEFAULT,MTI1:?MTI_LEN/bytes>>.    

%%-------------------------------------------------------------------------------------
%%-------------------------- Additional Header functions ------------------------------
%%-------------------------------------------------------------------------------------
-spec get_header_and_bitmap(MSG::binary())-> {HDR::binary(),PBTMP::binary(),SBTMP::binary()}.
get_header_and_bitmap(MSG)->
    <<HDR:?HDR_LEN/bytes,PBTMP:?BTMP_LEN/bytes,REST/bitstring>> = MSG,    
    <<SBTMP_FLG:1/bitstring,_REST_PBTMP/bitstring>> = hex:hex_to_bin(binary_to_list(PBTMP)),
    SBTMP = if
		SBTMP_FLG == <<1:1>> ->
		    <<SBTMP0:?BTMP_LEN/bytes,_REST/bitstring>> = REST,
		    SBTMP0;
		true -> none
	    end,
    {HDR,PBTMP,SBTMP}.

%% --------------------------------------------------------------------------------------------
%% ------------------------- Bitmap functions -------------------------------------------------
%% --------------------------------------------------------------------------------------------
-spec parse_bitmap(primary|secondary,BTMP::binary())-> map().
parse_bitmap(X,BTMP) ->
    ?LOG("Bitmap2=~p",[BTMP]),
    {FROM,TO}=case X of
		  primary -> {1,64};
		  secondary -> {65,128}
	      end,
    BTMP1 = hex:hex_to_bin(binary_to_list(BTMP)),
    BTL=[B || <<B:1>> <= BTMP1],
    {M,_}=lists:foldl(fun(ID,{MAP,[H|T]})->
			      ?LOG("parse_bitmap2: ID=~p,H=~p",[ID,H]),
			      case H of
				  1 -> 
				      {MAP#{ID => 1},T};
				  0 ->
				      {MAP,T}
			      end
		      end,{#{},BTL},lists:seq(FROM,TO)),
    M.

-spec generate_bitmap(FMAP::#{ID::integer() := #iso_field{}})-> binary().
generate_bitmap(FMAP)->
    %% 1 - first Secondary Bitmap...
    F1 = fun(ID,{SBTM,SBTM_LEN,SBTM_PRESENT})->
		 {SBTMX,SBTM_PRESENTX} = 
		     case maps:find(ID,FMAP) of
			 {ok,_V} -> {<<SBTM:SBTM_LEN/bitstring,1:1>>,(1 bor SBTM_PRESENT)};
			 _ ->  {<<SBTM:SBTM_LEN/bitstring,0:1>>,(0 bor SBTM_PRESENT)}
		     end,
		 {SBTMX,SBTM_LEN+1,SBTM_PRESENTX}
	 end,
    
    {SBTM,_,SBTM_PRESENT} = lists:foldl(F1,{<<>>,0,0},lists:seq(65,128)),

    %% 2 - Primary Bitmap
    FMAP1 = case SBTM_PRESENT of
		1 ->
		    FMAP#{1 := present};
		0 -> 
		    maps:remove(1,FMAP) %% profilaktycznie...
	    end,

    ?LOG("SEC-Bitmap present:~p~n",[SBTM_PRESENT]),
    
    F2 = fun(ID,{PBTM,PBTM_LEN})->
		 PBTMX = case maps:find(ID,FMAP1) of
			     {ok,_V} -> 
				 <<PBTM:PBTM_LEN/bitstring,1:1>>;
			     _ ->  
				 <<PBTM:PBTM_LEN/bitstring,0:1>>
			 end,
		 {PBTMX,PBTM_LEN+1}
	 end,
    
    {PBTM,_} = lists:foldl(F2,{<<>>,0},lists:seq(1,64)),
    case SBTM_PRESENT of
	1 ->
	    PBTM1=list_to_binary(hex:bin_to_hex(PBTM)),
	    SBTM1=list_to_binary(hex:bin_to_hex(SBTM)),
	    <<PBTM1:128/bitstring,SBTM1:128/bitstring>>;
	0 ->
	    PBTM1=list_to_binary(hex:bin_to_hex(PBTM)),
	    <<PBTM1:128/bitstring>>
    end.

%%-------------------------------------------------------------------------------------
%%-------------------------- Additional BITMAP functions ------------------------------
%%-------------------------------------------------------------------------------------
-spec get_bitmap(primary | secondary,MSG::binary())-> binary().
get_bitmap(primary,MSG) -> %% 16 HEX
    <<_PFX:?HDR_LEN/bytes,PBTM:?BTMP_LEN/bytes,_Rest/bitstring>> = MSG,
    PBTM;

get_bitmap(secondary,MSG) -> %% 16 HEX
    <<_PFX:?HDR_LEN/bytes,_PBTM:?BTMP_LEN/bytes,SBTM:?BTMP_LEN/bytes,_Rest/bitstring>> = MSG,
    SBTM.

-spec show_bitmap(BTMP::binary()|list())-> list().
show_bitmap(BTMP) when is_binary(BTMP)->
    show_bitmap(binary_to_list(BTMP));
show_bitmap(BTMP) when is_list(BTMP)->
    [X || <<X:1>> <= hex:hex_to_bin(BTMP)].

%%-------------------------------------------------------------------------------------
%%----------------------------- MESSAGE -----------------------------------------------
%%-------------------------------------------------------------------------------------
-spec build_message(#iso_msg{}, CFG::map()) -> BUFF::binary().
build_message(#iso_msg{prod=PROD,mti=MTI,fields=FIELDS},CFG) ->
    ?LOG("Start generating...",[]),

    F = fun(N,{BUFF,BUFF_LEN})->
		case maps:find(N,FIELDS) of
		    {ok,V} ->
			{_TAG,EXC,LEN,_,_,_} = iso_utils:get_field_defs(N,PROD,CFG),

			%%-------------------------------------------------------
			%% Dostosowanie do różnych form FMap
			%%-------------------------------------------------------
			Value = 
			    if 
				is_binary(V#iso_field.value) -> 
				    binary_to_list(V#iso_field.value);
				
				true -> V#iso_field.value
			    end,
			
			{F,Params} = case EXC of
					 fixed -> {fun generate_fixed/2,[Value,LEN]};
					 llvar -> {fun generate_llvar/2,[Value,LEN]};
					 lllvar -> {fun generate_lllvar/2,[Value,LEN]}
				     end,
			
			{FormattedVal,FormattedLen} = erlang:apply(F,Params),
			?LOG("Fld[~p]=[~p],Org_val=~p,exec=~p,BUFF_LEN=~p,LEN=~p",
			     [N,FormattedVal,Value,EXC,BUFF_LEN,LEN]),

			BUFF1 = <<BUFF:BUFF_LEN/binary,FormattedVal:FormattedLen/binary>>,
			?LOG("Curr BUFF=[~p]",[BUFF1]),

			{BUFF1,BUFF_LEN+FormattedLen};
		    _ ->
			?LOG("generator: skipping N=~p",[N]),
			{BUFF,BUFF_LEN}
		end
	end,
    
    {BUFF,_LX}=lists:foldl(F,{<<>>,0},lists:seq(2,128)), %% omijamy pole 1 == SEC-BTMT !    
    ?LOG("Generate: BUFF=~p",[BUFF]),
    HDR=generate_header(PROD,MTI),
    ?LOG("Generate: HDR=~p",[HDR]),
    BTMP=generate_bitmap(FIELDS),
    ?LOG("Generate: BTMP=~p",[BTMP]),

    <<HDR/binary,BTMP/binary,BUFF/binary>>.

%%---------------------------------------------------------------------------------------------
%% Parsed : mapa {N => #iso_field}
%%---------------------------------------------------------------------------------------------
-spec parse_fields(BUFF::binary(),
		   Off::integer(),
		   Flds::#{ID::integer() := FLD::#iso_field{}},
		   FldBitmap::map(),
		   HProd::atom(),
		   N::integer(),
		   CFG::map())-> {ok,FMAP::#{ID::integer() := FLD::#iso_field{}}}.
parse_fields(_,_,Flds,_,_,128,_) ->
    {ok,Flds};
parse_fields(BUFF,Off,Flds,FldBitmap,HPrd,N,CFG) ->
    case maps:find(N,FldBitmap) of
	{ok,_} ->
	    {TAG,EXC,LEN,DTYPE,
	     {DECTYPE,DEC},
	     {ENCTYPE,ENC}} = iso_utils:get_field_defs(N,HPrd,CFG),
	    
	    {F,Params} = case EXC of
			     fixed -> {fun parse_fixed/3,[BUFF,Off,LEN]};
			     llvar -> {fun parse_llvar/2,[BUFF,Off]};
			     lllvar -> {fun parse_lllvar/2,[BUFF,Off]}
			 end,
	    {Val,OffX}=erlang:apply(F,Params),
	    ?LOG("Fld[~p]=[~p]",[N,Val]),
	    
	    DefaultTag = if
			     N =< 64 ->
				 lists:flatten(io_lib:format("P~2..0w",[N]));
			     true ->
				 lists:flatten(io_lib:format("S~3..0w",[N]))
			 end,
	    
	    TAGX = if
		       TAG =:= none ->
			   DefaultTag;
		       true ->
			   TAG
		   end,
	    
	    DECODER = case {DECTYPE,DEC} of
			  {decoder,DEC} -> DEC;
			  {_,none} -> none;
			  {_,DECOMPOSITION1} ->
			      sub_field_decoder_gen(DECOMPOSITION1)
		      end,
	    ENCODER = case {ENCTYPE,ENC} of
			  {encoder,ENC} -> ENC;
			  {_,none} -> none;
			  {_,DECOMPOSITION2} ->
			      sub_field_encoder_gen(DECOMPOSITION2,DTYPE)
		      end,
	    
	    %% 2020-12-09
	    %%FLD=#iso_field{id=N,pos=(Off+LnPfx),lnpfx=LnPfx,dlen=LEN,value=Val},
	    FLD=#iso_field{id=N,tag=TAGX,value=Val,decoder=DECODER,encoder=ENCODER},
	    FldsX = Flds#{N => FLD},

	    parse_fields(BUFF,OffX,FldsX,FldBitmap,HPrd,N+1,CFG);
	
	error ->
	    ?LOG("Brak mapowania dla pola ~p",[N]),
	    parse_fields(BUFF,Off,Flds,FldBitmap,HPrd,N+1,CFG)
    end.

-spec parse_message(MSG::binary())-> {ok,#iso_msg{}}. 
parse_message(MSG) ->
    {hdr,MTI,HPRD,PBTMP,SBTMP} = parse_header(MSG),
    PMAP=parse_bitmap(primary,PBTMP),
    ?LOG("parse_bitmap: PMAP=~p",[lists:sort(maps:to_list(PMAP))]),
    case maps:find(1,PMAP) of
	{ok,_} ->
	    SMAP=parse_bitmap(secondary,SBTMP),
	    ?LOG("parse_bitmap: SMAP=~p",[lists:sort(maps:to_list(SMAP))]);
	error ->
	    SMAP=#{}
    end,

    Off = 32 + 1,

    MAP = maps:merge(PMAP,SMAP),

    {ok,Flds} = parse_fields(MSG,Off,#{},MAP,HPRD,1,iso_definitions:get_parser_config(?DIALECT)),

    %% Tokens...
    TKNS63 = case maps:find(63,Flds) of
		 {ok,P63} ->
		     parse_tokens(P63#iso_field.value);
		 _ ->
		     #{}
	     end,
    TKNS126 = case maps:find(126,Flds) of
		 {ok,S126} ->
		     parse_tokens(S126#iso_field.value);
		 _ ->
		     #{}
	     end,

    {ok,#iso_msg{org=MSG,fields=Flds,mti=binary_to_list(MTI),prod=HPRD,
		 tokens=maps:merge(TKNS63,TKNS126)}}.

%% Parse token data field (P63/S126) to token map
-spec parse_tokens(DATA::binary()) -> #{TAG::string() := #iso_token{}}.
parse_tokens(DATA)->
    try parse_header_token(DATA) of
	{_TLEN0,TNUM0,REST0}->
	    ?LOG("parse_tokens: TLEN=~p,TNUM=~p",[_TLEN0,TNUM0]),
	    F = fun(_N,{M,Data}=_ACC)->
			try parse_token_data(Data) of
			    {#iso_token{tag=TAG}=ISO_TKN,REST}->
				{M#{TAG => ISO_TKN},REST}
			catch
			    _:_ -> error(tkn_parsing_error)
			end
		end,

	    %% warto wykonac parsing tylko jesli TNUM0>1 ...
	    if
		TNUM0 > 1 ->
		    {TokenMap,_Rest}=lists:foldl(F,{#{},REST0},lists:seq(1,TNUM0-1)), %% bez TKH HDR...
		    ?LOG("parse_tokens: TokenMap=~p",[TokenMap]),
		    TokenMap;
		true ->
		    #{}
	    end
    catch
	C:E -> logger:error("parse_tokens: ERROR=~p:~p",[C,E]),
	       error(tkn_parsing_error)
    end.

-spec parse_header_token(DATA::binary()) -> {TLEN::integer(),TNUM::integer(),REST::binary()}.
parse_header_token(DATA)->
    ?LOG("parse_header_token: data=~p",[DATA]),
    <<"& ",TNUM:5/bytes,TLEN:5/bytes,REST/bitstring>> = DATA,
    {binary_to_integer(TLEN),binary_to_integer(TNUM),REST}.

%% Encode token map to field P63/S126 as binary...
-spec encode_tokens(#{TAG::string() := #iso_token{}}) -> binary().
encode_tokens(TKN_MAP)->
    ?LOG("encode_tokens: data=~p",[TKN_MAP]),
    TKNCNT0 = maps:size(TKN_MAP) + 1,
    TKNCNT = iso_utils:isofmt(TKNCNT0,fixed,5,"N"),

    L0 = maps:to_list(TKN_MAP),
    L = lists:sort(fun({A,_},{B,_})->
			   (A =< B)
		   end,L0),

    {TLEN0,BUFF} = lists:foldl(fun({_,#iso_token{tag=TAG,value=VAL}},{LEN,BUFF0})->
				       TAGBin=list_to_binary(TAG),
				       TLEN0=length(binary_to_list(VAL)),
				       TLEN=iso_utils:isofmt(TLEN0,fixed,5,"N"),
				       {TLEN0+10 + LEN,
					<<BUFF0/binary,"! ",TAGBin/binary,TLEN/binary," ",VAL/binary>>}
			       end,{0,<<"">>},L),
    TLEN1 = TLEN0 + 12, % header tkn len
    TLEN = iso_utils:isofmt(TLEN1,fixed,5,"N"),
    <<"& ",TKNCNT/binary,TLEN/binary,BUFF/binary>>.

-spec parse_token_data(DATA::binary())-> {#iso_token{},REST::binary()}.
parse_token_data(DATA)->
    ?LOG("parse_token_data: data=~p",[DATA]),
    <<"! ",TAG0:2/bytes,TLEN0:5/bytes," ",REST0/bitstring>> = DATA,
    TLEN = binary_to_integer(TLEN0),
    <<TKN:TLEN/bytes,REST/bitstring>> = REST0,
    TAG=binary_to_list(TAG0),
    DEC=case maps:find(TAG,iso_definitions:get_token_config()) of
	    {ok,#iso_token_def{decomposition=none}}->
		none;
	    {ok,#iso_token_def{decomposition=DECOMP}}->
		sub_field_decoder_gen(DECOMP);
	    _ ->
		none
	end,
    {#iso_token{tag=TAG,value=TKN,decoder=DEC},REST}.

%% Decode token value to items map.
-spec token_decode_items(#iso_token{})-> ITEMS_MAP::map().
token_decode_items(#iso_token{value=VAL,decoder=DEC}) ->
    if
	DEC =:= none ->
	    error(no_decoder); %% zamiast bledu matchowania w guard...
	true ->
	    ok
    end,
    DEC(VAL).

%% Encode token from items map to : {external binary serialization, #iso_token{value = VAL}}
-spec token_encode_items(TAG::string(),ITEMS_MAP::map())-> {EXTERNAL_FORMAT::binary(),#iso_token{}}.
token_encode_items(TAG,ITEMS_MAP)->
    ?LOG("encode_token: ~p, data=~p",[TAG,ITEMS_MAP]),
    TKNCFG = iso_definitions:get_token_config(),
    {ok,#iso_token_def{tag=TAG,decomposition=DECOMP}} = maps:find(TAG,TKNCFG),
    ?LOG("Decomposition for TKN-~p=~p",[TAG,DECOMP]),
    DEC=iso_parser:sub_field_decoder_gen(DECOMP),
    ENC=iso_parser:sub_field_encoder_gen(DECOMP,"ANS"),
    VAL=ENC(ITEMS_MAP),
    TLEN=iso_utils:isofmt(length(binary_to_list(VAL)),fixed,5,"N"),
    ?LOG("Decomposition for TKN-~p, VAL=~p, TLEN=~p",[TAG,VAL,TLEN]),
    TAGBin=list_to_binary(TAG),
    {<<"! ",TAGBin/binary,TLEN/binary," ",VAL/binary>>, #iso_token{tag=TAG,value=VAL,decoder=DEC}}.%items=ITEMS_MAP

%% Fild "decomposition" decoder generator
-spec sub_field_decoder_gen(list())-> fun((binary()) -> #{list() => binary()}).
sub_field_decoder_gen(SUBL)->
    fun(VAL)->
	    F = fun({TAG,LEN}=_SUB_FLD,{POS,M})->
			VSUB = binary:part(VAL,POS,LEN),
			MX = if
				 TAG =:= filler -> 
				     M;
				 true ->
				     M#{TAG => VSUB}
			     end,    
			{POS+LEN,MX}
		end,
	    {_POS1,M1}=lists:foldl(F,{0,#{}},SUBL),
	    M1
    end.	    

%% Fild "decomposition" encoder generator
-spec sub_field_encoder_gen(list(),string())-> fun((map()) -> binary()).
sub_field_encoder_gen(SUBL,DTYPE)->
    fun(MAP)->
	    F = fun({TAG,LEN}=_SUB_FLD,BUFF)->
			V1 = case maps:find(TAG,MAP) of
				 {ok,V} ->
				     V;
				 _ ->
				     ""
			     end,
			V2=iso_utils:isofmt(V1,fixed,LEN,DTYPE),
			?LOG("encoder: BUFF=~p,V2=~p",[BUFF,V2]),
			<<BUFF/binary,V2/binary>>
		end,
	    lists:foldl(F,<<"">>,SUBL)
    end.	    

-spec field_decode_items(#iso_field{})-> ITEMS_MAP::map().
field_decode_items(#iso_field{value=VAL,decoder=DEC}) ->
    if
	DEC =:= none ->
	    error(no_decoder); %% zamiast bledu matchowania w guard...
	true ->
	    ok
    end,
    DEC(VAL).

-spec field_encode_items(#iso_field{},map())-> #iso_field{}.
field_encode_items(#iso_field{encoder=ENC}=FLD,ITEMS_MAP) ->
    if
	ENC =:= none ->
	    error(no_encoder); %% zamiast bledu matchowania w guard...
	true ->
	    ok
    end,
    FLD#iso_field{value=ENC(ITEMS_MAP)}.
