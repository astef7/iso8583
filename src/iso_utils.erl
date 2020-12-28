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
%% ISO8583 / BIC ISO Parser Utilities
%%------------------------------------------------------------------------------
%% 2018-2020
%%==============================================================================
-module(iso_utils).
-vsn("3.0.11").
-include("iso_defs.hrl").

-compile(export_all).


-define(RRN_LEN,12).
-define(STAN_LEN,6).

-define(MTI_OFFSET,12).
-define(MTI_LEN,4).

-define(DIALECT,biciso).


-ifdef(debug).
-define(LOG(FMT,VALS),io:format(FMT++"\n",VALS)).
-else.
-define(LOG(FMT,VALS),ok).
-endif.

%%--------------------------------------------------------------------------
%%------------ Functions operating by offset, no parsing -------------------
%%--------------------------------------------------------------------------

%%--------------------------- MTI ------------------------------------------
-spec get_mti(MSG :: binary()) -> {Pfx :: binary(), MTI :: binary(), Rst :: binary()}.
get_mti(MSG) ->
    <<Pfx:?MTI_OFFSET/binary,MTI:?MTI_LEN/binary,Rst/bitstring>> = MSG,
    {Pfx,MTI,Rst}.

-spec set_mti(MTI :: binary(), MSG :: binary()) -> binary().
set_mti(MTI,MSG) ->
    <<Pfx:?MTI_OFFSET/binary,_MTI_ORG:?MTI_LEN/binary,Rst/bitstring>> = MSG,
    <<Pfx:?MTI_OFFSET/binary,MTI:?MTI_LEN/binary,Rst/bitstring>>.

%%--------------------------- RRN ------------------------------------------
-spec get_rrn({offset,OFF::integer()},MSG :: binary()) -> binary().
get_rrn({offset,OFF},MSG) ->
    <<_Prefix:OFF/binary,RRN:?RRN_LEN/binary,_Rest/bitstring>> = MSG,
    RRN.

-spec set_rrn({int | bin, N :: integer() | binary()}, 
	      {offset,OFF::integer()},
	      MSG :: binary()) -> {RRN :: binary(),Out :: binary()}.
set_rrn({int,N},{offset,OFF},MSG) when is_integer(N) ->
    set_rrn({bin,format_integer(N,?RRN_LEN)},{offset,OFF},MSG);

set_rrn({bin,B},{offset,OFF},MSG) when is_binary(B) ->
    ?LOG("set_rrn/bin: B=~p",[B]),
    LEN = length(binary_to_list(B)),
    if 
	LEN =:= ?RRN_LEN ->
	    ok;
	true -> 
	    error(bad_rrn_length)
    end,
    <<Prefix:OFF/binary,_ORRN:?RRN_LEN/binary,Rest/bitstring>> = MSG,
    Out = <<Prefix/binary,B/binary,Rest/bitstring>>,
    ?LOG("set_rrn/bin: MSG0=~p~n",[MSG]),
    ?LOG("set_rrn/bin: MSG1=~p~n",[Out]),
    {B,Out}.

%%--------------------------- STAN -----------------------------------------
-spec get_stan({offset,OFF::integer()},MSG :: binary()) -> STAN::binary().
get_stan({offset,OFF},MSG) ->
    <<_Prefix:OFF/binary,STAN:?STAN_LEN/binary,_Rest/bitstring>> = MSG,
    STAN.

-spec set_stan({int | bin, N::integer() | binary()}, {offset,OFF::integer()}, MSG::binary()) -> 
	  {STAN::binary(),MSG::binary()}.
set_stan({int,N},{offset,OFF},MSG) when is_integer(N) ->
    set_stan({bin,format_integer(N,?STAN_LEN)},{offset,OFF},MSG);

set_stan({bin,B},{offset,OFF},MSG) when is_binary(B) ->
    LEN = length(binary_to_list(B)),
    if 
	LEN =:= ?STAN_LEN ->
	    ok;
	true -> 
	    error(bad_stan_length)
    end,
    <<Prefix:OFF/binary,_OSTAN:?STAN_LEN/binary,Rest/bitstring>> = MSG,
    Out = <<Prefix/binary,B/binary,Rest/bitstring>>,
    {B,Out}.

%%--------------------------------------------------------------------------
%%--------- Functions operating in #iso_mag{}, after parsing ---------------
%%--------------------------------------------------------------------------

%%--------------------------- PAN / EXPDT ----------------------------------
-spec get_pan_expdt(#iso_msg{})-> {PAN::binary(),EXPDT::binary()} | error.
get_pan_expdt(#iso_msg{fields=FIELDS})->
    case maps:find(35,FIELDS) of
	{ok,V} ->
	    #iso_field{value = P35, decoder = DEC} = V,
	    M = case DEC of
		    none ->
			[PAN0,ADD]=binary:split(P35,<<"=">>),
			<<_CNTRY:3/bytes,EXPDT0:4/bytes,_RST/binary>> = ADD,
			#{"PAN" => PAN0, "EXPDT" => EXPDT0};
		    DEC ->
			DEC(P35)
		end,
	    #{"PAN" := PAN, "EXPDT" := EXPDT} = M,
	    {PAN,EXPDT};
	_ -> error
    end.

%% zwraca nowy ISO_MSG
-spec set_pan_expdt(PAN0::binary() | string(),EXPDT0::binary() | string(), #iso_msg{})-> #iso_msg{}.
set_pan_expdt(PAN0,EXPDT0,#iso_msg{fields=FIELDS}=ISO_MSG) ->
    PAN = if
	      is_list(PAN0) -> list_to_binary(PAN0);
	      true -> PAN0
	  end,
    EXPDT = if
		is_list(EXPDT0) -> list_to_binary(EXPDT0);
		true -> EXPDT0
	    end,

    %% może jest templejt ...
    P35_NEW = case maps:find(35,FIELDS) of
	    {ok,V} ->
		#iso_field{value = P35} = V,
		<<_PAN_OLD:16/bytes,IGNORE:4/bytes,_EXPDT_OLD:4/bytes,Rest/binary>> = P35,
		%% zamiana...
		<<PAN:16/bytes,IGNORE:4/bytes,EXPDT:4/bytes,Rest/binary>>;
	    _ -> 
		%% w takim razie, trzeba użyć defaultów...
		<<PAN:16/bytes,"=616",EXPDT:4/bytes,"701777">>
	    end,
    set_field_value(35,P35_NEW,ISO_MSG). 
%%------------------------- Transmission DT ----------------------------------------------
-spec set_transmission_dt(DT::list()|binary(),#iso_msg{})-> #iso_msg{}.
set_transmission_dt(MMDDhhmmss,ISO_MSG) when is_record(ISO_MSG,iso_msg) ->
    VAL = if
	      is_list(MMDDhhmmss) -> list_to_binary(MMDDhhmmss);
	      true -> MMDDhhmmss
	  end,
    set_field_no_fmt(7,VAL,ISO_MSG).

%%------------------------- Generic Get/Set by fld.no ------------------------------------
-spec get_field(N::integer(),MSG::#iso_msg{})-> {ok,FLD::#iso_field{}} | error.
get_field(N,#iso_msg{fields=FIELDS}=_ISO_MSG)->
    maps:find(N,FIELDS).

-spec get_field_value(N::integer(),MSG::#iso_msg{})-> {ok,VAL::binary()} | error.
get_field_value(N,#iso_msg{fields=FIELDS}=_ISO_MSG)->
    case maps:find(N,FIELDS) of
	{ok,#iso_field{value = VAL}} -> {ok,VAL};
	_ -> error
    end.

%% Ustawienie pola N wartością VAL
%% Zwraca nowe ISO_MSG
-spec set_field_value(N::integer(),VAL::binary(),#iso_msg{})-> #iso_msg{}.
set_field_value(N,VAL,#iso_msg{fields=FIELDS,prod=PROD}=ISO_MSG)->
    {TAG,EXC,LEN,DTYPE,_,_} = get_field_defs(N,PROD,iso_definitions:get_parser_config(?DIALECT)),
    FIELDS1 = case maps:find(N,FIELDS) of
		  {ok,V} when is_record(V,iso_field) ->
		      FIELDS#{N => V#iso_field{value=isofmt(VAL,EXC,LEN,DTYPE)}};
		  _ ->
		      ?LOG("Inserting field ~p into FMap",[N]),
		      FIELDS#{N => #iso_field{id=N,tag=TAG,value=isofmt(VAL,EXC,LEN,DTYPE)}}
	      end,
    ISO_MSG#iso_msg{fields=FIELDS1}.

%% Wersja BEZ FORMATOWANIA wartości VAL. VAL musi być podane dokładnie tak, jak wymaga pole
-spec set_field_no_fmt(N::integer(),VAL::binary(),#iso_msg{})-> #iso_msg{}.
set_field_no_fmt(N,VAL,#iso_msg{fields=FIELDS,prod=PROD}=ISO_MSG)->
    {TAG,_EXC,_LEN,_DTYPE,_,_} = get_field_defs(N,PROD,iso_definitions:get_parser_config(?DIALECT)),
    FIELDS1 = case maps:find(N,FIELDS) of
		  {ok,V} when is_record(V,iso_field) ->
		      FIELDS#{N => V#iso_field{value = VAL}};
		  _ ->
		      ?LOG("Inserting field ~p into FMap",[N]),
		      FIELDS#{N => #iso_field{id=N,tag=TAG,value=VAL}}
	      end,
    ISO_MSG#iso_msg{fields=FIELDS1}.

%%------------------------------------------------------------------------------
%%------------------------ Utilities -------------------------------------------
%%------------------------------------------------------------------------------

%% sformatowanie N na Len pozycjach z wiodącymi zerami
-spec format_integer(N::integer(),Len::integer())-> binary().
format_integer(N,Len) when is_integer(Len) ->
    FMT = "~"++integer_to_list(Len)++"..0B",
    list_to_binary(io_lib:format(FMT,[N])).

%% A to starsza wersja jaką stosowałem:
%% lists:flatten(io_lib:format("~12..0s",[integer_to_list(N,10)])))

%% Formatuje wyłącznie VAL, nie dokleja prefixu dla LL/LLL
-spec isofmt(VAL0::list() | binary() | integer(),
	     EXC::iso_fmt_type(),
	     LEN::integer(),
	     DTYPE::list())-> 
	  binary().
isofmt(VAL0,EXC,LEN,DTYPE) when is_list(VAL0) or is_binary(VAL0) or is_integer(VAL0) ->
    VAL = if 
	      is_binary(VAL0) -> binary_to_list(VAL0);
	      true -> VAL0
	  end,

    if 
	is_list(VAL) and (length(VAL) > LEN) -> error(maxlen_exceeded);
	true -> ok
    end, 
    
    case {DTYPE,EXC} of
	{"N",fixed} ->
	    FMT = "~"++integer_to_list(LEN)++"..0B";
	{"X+N",fixed} ->
	    FMT = "~"++integer_to_list(LEN)++"..0B";
	{"AN",fixed} ->
	    FMT = "~-"++integer_to_list(LEN)++".. s";
	{"ANS",fixed} ->
	    FMT = "~-"++integer_to_list(LEN)++".. s";

	%% dalej formatowanie dla LL/LLL...
	{"AN",_} ->
	    FMT = "~s";
	{"ANS",_} ->
	    FMT = "~s"
    end,
    list_to_binary(lists:flatten(io_lib:format(FMT,[VAL]))).

-spec dump(MSG::#iso_msg{}) -> string().
dump(#iso_msg{fields=FIELDS,prod=_PROD})->

    %% Fields can be annotated with description from definition : _PROD & _CFG is required in this case
    CFG=iso_definitions:get_parser_config(biciso),

    F = fun(N,DUMP)->
		case maps:find(N,FIELDS) of
		    {ok,#iso_field{id=ID,tag=TAG,value=VAL}} ->

			%% Field description from definitions, ignorint PROD...
			DESC = case maps:find(ID,CFG) of
				   {ok, #iso_field_def{name=NAM}}->
				       NAM;
				   _ -> 
				       ""
			       end,

			DUMP++
			    lists:flatten(io_lib:format("[~3..0B]",[ID]))++
			    lists:flatten(io_lib:format("[~-12.12s]",[TAG]))++
			    " = ["++binary_to_list(VAL)++"]"++
			    DESC++"\n";
		    _ -> DUMP
		end
	end,
    DUMPX = lists:foldl(F,"",lists:seq(1,128)),
    %% logger:info("Dump:~n~s",[DUMPX]).
    DUMPX.

-spec get_variant(VL::[#variant{}],Prod::atom())-> #variant{}.
get_variant([],_) ->
    none;
get_variant([H|T],Variant) ->
    {VX,V}=H,
    if
	Variant =:= VX ->
	    V;
	true ->
	    get_variant(T,Variant)
    end.

-spec get_field_defs(N::integer(),Prod::atom(),CFG::map())-> 
	  {TAG::list() | none,
	   EXC::atom(),
	   LEN::integer(),
	   DTYPE::string(),
	   {decoder,fun((VAL::binary())->ITEMS::map())|none} | {decomposition,DECOMP::list()|none},
	   {encoder,fun((ITEMS::map())->VAL::binary())|none} | {decomposition,DECOMP::list()|none}}.
get_field_defs(N,Prod,CFG)->
    #{N := #iso_field_def{tag=TAG0,variants=VL,fmt=EXC0,params=PARAMS0,
			  decoder=DEC0,encoder=ENC0,decomposition=DECOMPOSE0}=_FLD_DEF} = CFG,

    if
	is_list(VL) ->
	    #variant{tag=TAG1,fmt=EXC1,params=#fmt_params{len=LEN1,content=DTYPE1},
		     decoder=DEC1,encoder=ENC1,decomposition=DECOMPOSE1} = get_variant(VL,Prod),
	    DECX = case {DEC0,DEC1,DECOMPOSE0,DECOMPOSE1} of
		       {none,none,DECOM0D,none} -> {decomposition,DECOM0D};
		       {none,none,_,DECOM1D} -> {decomposition,DECOM1D}; 
		       {D0,none,_,_} -> {decoder,D0};
		       {_,D1,_,_} -> {decoder,D1}
		       end,
	    ENCX = case {ENC0,ENC1,DECOMPOSE0,DECOMPOSE1} of
		       {none,none,DECOM0E,none} -> {decomposition,DECOM0E};
		       {none,none,_,DECOM1E} -> {decomposition,DECOM1E}; 
		       {E0,none,_,_} -> {encoder,E0};
		       {_,E1,_,_} -> {encoder,E1}
		       end,
	    {TAG1,EXC1,LEN1,DTYPE1,DECX,ENCX};
	true ->
	    %% dopiero na tym etapie wolno matchować PARAMS...
	    #fmt_params{len=LEN0,content=DTYPE0} = PARAMS0,
	    DECX = case {DEC0,DECOMPOSE0} of
		       {none,DECOM0D} -> {decomposition,DECOM0D};
		       {DEC0,_} -> {decoder,DEC0}
		   end,
	    ENCX = case {ENC0,DECOMPOSE0} of
		       {none,DECOM0E} -> {decomposition,DECOM0E};
		       {ENC0,_} -> {encoder,ENC0}
		   end,

	    {TAG0,EXC0,LEN0,DTYPE0,DECX,ENCX}
    end.

%% DEPRECATED: currently preferred way is via decoder/encoder...
-spec decode_S90(S90::binary())-> 
	  {OrgTxType::binary(),
	   OrgSEQ::binary(),
	   TxDate::binary(),
	   TxTime::binary(),
	   OrgCaptureDate::binary()}.
decode_S90(S90)->
    <<OrgTxType:4/bytes,OrgSEQ:12/bytes,TxDate:4/bytes,TxTime:8/bytes,OrgCaptureDate:4/bytes,
      _Filler:10/bytes>> = S90,    
    {OrgTxType,OrgSEQ,TxDate,TxTime,OrgCaptureDate}.

%% DEPRECATED: currently preferred way is via decoder/encoder...
-spec decode_S95(atm|pos,S95::binary())->
	  {ActTxAmt::binary(),
	   StlAmt::binary()|none,
	   TxFee::binary()|none,
	   StlFee::binary()|none}.
decode_S95(atm,S95)->
    <<ActTxAmt:12/bytes,StlAmt:12/bytes,TxFee:9/bytes,StlFee:9/bytes>> = S95,    
    {ActTxAmt,StlAmt,TxFee,StlFee};
decode_S95(pos,S95)->
    <<ActTxAmt:12/bytes,_NotUsed:30/bytes>> = S95,    
    {ActTxAmt,none,none,none}.

%% DEPRECATED: currently preferred way is via decoder/encoder...
-spec decode_S70(S70::binary()|list())-> string().
decode_S70(S70)->
    S70L = if
	       is_binary(S70) ->
		   binary_to_list(S70);
	       true -> S70
	   end,
    case S70L of
	"001" -> "SIGN-ON";
	"002" -> "SIGN-OFF";
	"301" -> "ECHO"
    end.
