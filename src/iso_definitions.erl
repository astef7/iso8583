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
%% ISO8583 / BIC ISO Parser Fields & Token definitions
%%------------------------------------------------------------------------------
%% 2018-2020
%%==============================================================================
-module(iso_definitions).
-export([get_parser_config/1]).
-export([get_token_config/0]).

-include("iso_defs.hrl").

%%===================================================================================================
%% BIC-ISO 87 Definitions
%%===================================================================================================
-spec get_parser_config(atom()) -> #{integer() => #iso_field_def{}}.
get_parser_config(biciso)->
    #{
      00 => #iso_field_def{id=00,name="Primary Bitmap",tag=none,fmt=none,
			   params=#fmt_params{content="AN",len=16},variants=none},
      01 => #iso_field_def{id=01,name="Secondary Bitmap",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=16},variants=none},
      02 => #iso_field_def{id=02,name="PAN",tag=none,fmt=llvar,
			   params=#fmt_params{content="N",len=19},variants=none},
      03 => #iso_field_def{id=03,name="Processing Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=6},variants=none},
      04 => #iso_field_def{id=04,name="Transaction Amount",tag="TX-AMT",fmt=fixed,
			   params=#fmt_params{content="N",len=12},variants=none},
      05 => #iso_field_def{id=05,name="Settlement Amount",tag="STL-AMT",fmt=fixed,
			   params=#fmt_params{content="N",len=12},variants=none},
      06 => #iso_field_def{id=06,name="Cardholder Billing Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=12},variants=none},
      07 => #iso_field_def{id=07,name="Transmission Date Time",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=10},variants=none},
      08 => #iso_field_def{id=08,name="Cardholder Billing Fee Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=8},variants=none},
      09 => #iso_field_def{id=09,name="Settlement Conversion Rate",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=8},variants=none},
      10 => #iso_field_def{id=10,name="Cardholder Billing Conversion Rate",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=8},variants=none},
      11 => #iso_field_def{id=11,name="System Trace Audit Number",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=6},variants=none},
      12 => #iso_field_def{id=12,name="Local Transaction Time",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=6},variants=none},
      13 => #iso_field_def{id=13,name="Local Transaction Date",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      14 => #iso_field_def{id=14,name="Expiration Date",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      15 => #iso_field_def{id=15,name="Settlement Date",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      16 => #iso_field_def{id=16,name="Conversion Date",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      17 => #iso_field_def{id=17,name="Capture Date",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      18 => #iso_field_def{id=18,name="Merchant Type",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=4},variants=none},
      19 => #iso_field_def{id=19,name="Acquirer Institution Country Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      20 => #iso_field_def{id=20,name="Country Code PAN Extended",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      21 => #iso_field_def{id=21,name="Forwarding Institution Country Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      22 => #iso_field_def{id=22,name="POS Entry Mode",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      23 => #iso_field_def{id=23,name="Card Sequence Number",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      24 => #iso_field_def{id=24,name="Network International Id",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      25 => #iso_field_def{id=25,name="POS Condition Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=2},variants=none},
      26 => #iso_field_def{id=26,name="POS PIN Capture Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=2},variants=none},
      27 => #iso_field_def{id=27,name="Auth Identification Response Len",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=1},variants=none},
      28 => #iso_field_def{id=28,name="Transaction Fee Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="X+N",len=8},variants=none},
      29 => #iso_field_def{id=29,name="Settlement Fee Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="X+N",len=8},variants=none},
      30 => #iso_field_def{id=30,name="Transaction Processing Fee Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="X+N",len=8},variants=none},
      31 => #iso_field_def{id=31,name="Settlement Processing Fee Amount",tag=none,fmt=fixed,
			   params=#fmt_params{content="X+N",len=8},variants=none},
      32 => #iso_field_def{id=32,name="AIIC",tag=none,fmt=llvar,
			   params=#fmt_params{content="N",len=11},variants=none},
      33 => #iso_field_def{id=33,name="Forwarding Institution Identification Code",tag=none,fmt=llvar,
			   params=#fmt_params{content="N",len=11},variants=none},
      34 => #iso_field_def{id=34,name="Extended PAN",tag=none,fmt=llvar,
			   params=#fmt_params{content="AN",len=28},variants=none},
      35 => #iso_field_def{id=35,name="Track 2 Data",tag="TR2DT",fmt=llvar,
			   params=#fmt_params{content="ANS",len=37},variants=none,
			   decoder=
			       fun(VAL)->
				       [PAN,ADD]=binary:split(VAL,<<"=">>),
				       <<_CNTRY:3/bytes,EXPDT:4/bytes,_RST/binary>> = ADD,
				       #{"PAN" => PAN, "EXPDT" => EXPDT}
			       end,
			   encoder=
			       fun(#{"PAN" := PAN, "EXPDT" := EXPDT}=_ITEMS)->
				       <<PAN/binary,"=616",EXPDT/binary,"701777">>
			       end
			  },
      36 => #iso_field_def{id=36,name="Track 3 Data",tag=none,fmt=llvar,
			   params=#fmt_params{content="ANS",len=104},variants=none},
      37 => #iso_field_def{id=37,name="RRN",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=12},variants=none},
      38 => #iso_field_def{id=38,name="Authorization Identification Response",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=6},variants=none},
      39 => #iso_field_def{id=39,name="Response Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=2},variants=none},
      40 => #iso_field_def{id=40,name="Service Restriction Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=3},variants=none},
      41 => #iso_field_def{id=41,name="Card Acceptor Terminal Identification",tag=none,fmt=fixed,
			   params=#fmt_params{content="ANS",len=16},variants=none},
      42 => #iso_field_def{id=42,name="Card Acceptor Identification Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="ANS",len=15},variants=none},
      43 => #iso_field_def{id=43,name="Card Acceptor Name Location",tag=none,fmt=fixed,
			   params=#fmt_params{content="ANS",len=40},
			   %% decoder=fun(VAL)->
			   %% 		   <<TrmOwner:22/bytes,
			   %% 		     TrmCity:13/bytes,
			   %% 		     _TrmState:3/bytes,
			   %% 		     TrmCntry:2/bytes>> = VAL,
			   %% 		   #{"TRM-OWNER" => TrmOwner, 
			   %% 		     "TRM-CNTRY" => TrmCntry,
			   %% 		     "TRM-CITY" => TrmCity}
			   %% 	   end,
			   decomposition=[
					  {"TRM-OWNER",22},
					  {"TRM-CITY",13},
					  {filler,3},
					  {"TRM-CNTRY",2}
					 ],
			   variants=[
				     {atm,#variant{name="Card Acceptor Name Location",
						   tag="ACPT-NAM-LOC",fmt=fixed,
						   params=#fmt_params{content="ANS",len=40}}},
				     {pos,#variant{name="Card Acceptor Name Location",
						   tag="ACPT-NAM-LOC",fmt=fixed,
						   params=#fmt_params{content="ANS",len=40}}},
				     {teller,#variant{name="Card Acceptor Name Location",
						      tag=none,fmt=llvar,
						      params=#fmt_params{content="ANS",len=8}}}
				    ]},
      44 => #iso_field_def{id=44,name="Additional Response Data",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Additional Response Data",
						   tag=none,fmt=llvar,
						   params=#fmt_params{content="ANS",len=25}}},
				     {pos,#variant{name="Additional Response Data",
						   tag=none,fmt=llvar,
						   params=#fmt_params{content="ANS",len=2}}},
				     {telebanking,#variant{name="Additional Response Data",
							   tag=none,fmt=llvar,
							   params=#fmt_params{content="ANS",len=8}}}
				    ]},
      45 => #iso_field_def{id=45,name="Track 1 Data",tag=none,fmt=llvar,
			   params=#fmt_params{content="ANS",len=76},variants=none},
      46 => #iso_field_def{id=46,name="ISO Additional Data",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=999},variants=none},
      47 => #iso_field_def{id=47,name="National Additional Data",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=999},variants=none},
      48 => #iso_field_def{id=48,name="National Additional Data",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Additional Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=44}}},
				     {fhm,#variant{name="Additional Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=76}}},
				     {pos,#variant{name="Retailer Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=44}}}, %% Uwaga: w SPEC jest len=27, ale nie zgadza się z realnym komunikatem z B24
				     {telebanking,#variant{name="Additional Data",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=200}}},
				     {teller,#variant{name="Routing Data",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=47}}},
				     {netmgt,#variant{name="Additional Data",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=17}}} %% UWAGA : w spec było LEN=6 ale to nie zgadza się z danymi z B24
				    ]},
      49 => #iso_field_def{id=49,name="Transactin Currency Code",tag="TX-CRCD",fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      50 => #iso_field_def{id=50,name="Settlement Currency Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      51 => #iso_field_def{id=51,name="Cardholder Billing Currency Code",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=3},variants=none},
      52 => #iso_field_def{id=52,name="PIN Data",tag=none,fmt=fixed,
			   params=#fmt_params{content="AN",len=16},variants=none},
      53 => #iso_field_def{id=53,name="Security Control Info",tag=none,fmt=fixed,
			   params=#fmt_params{content="N",len=16},variants=none},
      54 => #iso_field_def{id=54,name="Additional Amounts",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Additional Amounts",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=12}}},
				     {pos,#variant{name="Additional Amounts",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=12}}},
				     {telebanking,#variant{name="Additional Amounts",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=120}}}
				    ]},
      55 => #iso_field_def{id=55,name="ISO Reserved",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=999},variants=none},
      56 => #iso_field_def{id=56,name="ISO Reserved",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=999},variants=none},
      57 => #iso_field_def{id=57,name="National Reserved",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=999},variants=none},
      58 => #iso_field_def{id=58,name="Financial Token",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=132},variants=none},
      59 => #iso_field_def{id=59,name="CAF Update Token",tag=none,fmt=lllvar,
			   params=#fmt_params{content="ANS",len=14},variants=none},
      60 => #iso_field_def{id=60,name="Terminal Data",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Terminal Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=12}}},
				     {fhm,#variant{name="Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=58}}},
				     {pos,#variant{name="Terminal Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=16}}},
				     {telebanking,#variant{name="Acquirer Data",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=10}}},
				     {teller,#variant{name="Acquirer Data",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=66}}}
				    ]},
      61 => #iso_field_def{id=61,name="Card Issuer and Authorizer data",
			   tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Card Issuer and Authorizer Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=13}}},
				     {fhm,#variant{name="User Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=100}}},
				     {pos,#variant{name="Card Issuer Category Response Code Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=19}}},
				     {telebanking,#variant{name="Issuer Institution Data",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=15}}},
				     {teller,#variant{name="Request Header Data",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=50}}}
				    ]},
      62 => #iso_field_def{id=62,name="Postal Code",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="Postal Code",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=10}}},
				     {fhm,#variant{name="CAF Exponent",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=1}}},
				     {pos,#variant{name="Postal Code",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=10}}},
				     {telebanking,#variant{name="Reccuring Transaction Data",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=7}}},
				     {teller,#variant{name="Request Header Data",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=5}}}
				    ]},
      63 => #iso_field_def{id=63,name="PIN Offset",tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="PIN Offset",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=16}}},
				     {fhm,#variant{name="Enhanced Preauthorized Hold",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=945}}},
				     {pos,#variant{name="Additional Data",
						   tag=none,fmt=lllvar,
						   params=#fmt_params{content="ANS",len=597}}},
				     {telebanking,#variant{name="Special Data",
							   tag=none,fmt=lllvar,
							   params=#fmt_params{content="ANS",len=33}}},
				     {teller,#variant{name="NBF Token",
						      tag=none,fmt=lllvar,
						      params=#fmt_params{content="ANS",len=554}}}
				    ]},
      64 => #iso_field_def{id=64,name="Primary Message Authentication Code",
			   tag=none,fmt=fixed,
			   params=#fmt_params{content="ANS",len=16},variants=none},
      
      %%----------------------------------------------------------------------------------------------
      %% Secondary Bitmap ...
      %%----------------------------------------------------------------------------------------------
      
      65 => #iso_field_def{id=66,name="Extended Bitmap - NOT USED",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=8},variants=none},
      67 => #iso_field_def{id=67,name="Extended Payment Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=2},variants=none},
      68 => #iso_field_def{id=68,name="Receiving Institution Country Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=3},variants=none},
	  69 => #iso_field_def{id=69,name="Settlement Institution Country Code",
			       tag=none,fmt=fixed,params=#fmt_params{content="N",len=3},variants=none},
      70 => #iso_field_def{id=70,name="Net Mgt Information Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=3},variants=none},
      %% 001 - Logon
      %% 002 - Logoff
      %% 161 - Change Key
      %% 162 - New key
      %% 163 - Repeat key
      %% 164 - Verify key
      %% 301 - Echo test
      71 => #iso_field_def{id=71,name="Message Number",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=4},variants=none},
      72 => #iso_field_def{id=72,name="Message Number Last",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=4},variants=none},
      73 => #iso_field_def{id=73,name="Action Date", %% YYMMDD
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=6},variants=none}, 
      74 => #iso_field_def{id=74,name="Number Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      75 => #iso_field_def{id=75,name="Reversal Number Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      76 => #iso_field_def{id=76,name="Number Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      77 => #iso_field_def{id=77,name="Reversal Number Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      78 => #iso_field_def{id=78,name="Number Transfer",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      79 => #iso_field_def{id=79,name="Reversal Number Transfer",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      80 => #iso_field_def{id=80,name="Number Inquiries",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      81 => #iso_field_def{id=81,name="Number Authorizations",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=10},variants=none},
      82 => #iso_field_def{id=82,name="Processing Fee Amount Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=12},variants=none},
      83 => #iso_field_def{id=83,name="Transaction Fee Amount Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=12},variants=none},
      84 => #iso_field_def{id=84,name="Processing Fee Amount Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=12},variants=none},
      85 => #iso_field_def{id=85,name="Transaction Fee Amount Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=12},variants=none},
      86 => #iso_field_def{id=86,name="Amount Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=16},variants=none},
      87 => #iso_field_def{id=87,name="Reversal Amount Credits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=16},variants=none},
      88 => #iso_field_def{id=88,name="Amount Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=16},variants=none},
      89 => #iso_field_def{id=89,name="Reversal Amount Debits",
			   tag=none,fmt=fixed,params=#fmt_params{content="N",len=16},variants=none},
      90 => #iso_field_def{id=90,name="Original Data Elements",
			   tag=none,fmt=none,params=none,
			   %% decoder=fun(VAL)->
			   %% 		   <<OrgMTI:4/bytes,
			   %% 		     OrgSeq:12/bytes,
			   %% 		     OrgTxDate:4/bytes,
			   %% 		     OrgTxTime:8/bytes,
			   %% 		     OrgCaptureDate:4/bytes,
			   %% 		     _Filler/binary>> = VAL,
			   %% 		   #{"ORG-MTI" => OrgMTI, 
			   %% 		     "ORG-SEQ" => OrgSeq,
			   %% 		     "ORG-TX-DATE" => OrgTxDate,
			   %% 		     "ORG-TX-TIME" => OrgTxTime,
			   %% 		     "ORG-CAP-DATE" => OrgCaptureDate}
			   %% 	   end,
			   decomposition=[
					  {"ORG-MTI",4},
					  {"ORG-SEQ",12},
					  {"ORG-TX-DATE",4},
					  {"ORG-TX-TIME",8},
					  {"ORG-CAP-DATE",4},
					  {filler,10}
					 ],
			   variants=[
				     {atm,#variant{name="",
						   tag=none,fmt=fixed,
						   params=#fmt_params{content="ANS",len=42}}},
				     {fhm,#variant{name="",
						   tag=none,fmt=fixed,
						   params=#fmt_params{content="ANS",len=42}}},
				     {pos,#variant{name="",
						   tag=none,fmt=fixed,
						   params=#fmt_params{content="ANS",len=42}}},
				     {telebanking,#variant{name="",
							   tag=none,fmt=fixed,
							   params=#fmt_params{content="ANS",len=42}}},
				     {teller,#variant{name="",
						      tag=none,fmt=fixed,
						      params=#fmt_params{content="ANS",len=42}}}
				    ]},
      91 => #iso_field_def{id=91,name="File Update Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=1},variants=none},
      92 => #iso_field_def{id=92,name="File Security Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=2},variants=none},
      93 => #iso_field_def{id=93,name="Response Indicator",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=5},variants=none},
      94 => #iso_field_def{id=94,name="Service Indicator",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=7},variants=none},
      95 => #iso_field_def{id=95,name="Replacement Amounts",
			   tag=none,fmt=none,params=none,
			   variants=[
				     {atm,#variant{name="",
						   tag=none,fmt=fixed,
						   params=#fmt_params{content="AN",len=42},
						   %% decoder=fun(VAL)->
						   %% 		   <<ActTxAmt:12/bytes,
						   %% 		     StlAmt:12/bytes,
						   %% 		     TxFee:9/bytes,
						   %% 		     StlFee:9/bytes,
						   %% 		     _Rest/binary>> = VAL,
						   %% 		   #{"ACT-TX-AMT" => ActTxAmt, 
						   %% 		     "STL-AMT" => StlAmt,
						   %% 		     "TX-FEE" => TxFee,
						   %% 		     "STL-FEE" => StlFee}
						   %% 	   end},
				      decomposition=[
						     {"ACT-TX-AMT",12},
						     {"STL-AMT",12},
						     {"TX-FEE",9},
						     {"STL-FEE",9}
						    ]}},				     
				     {pos,#variant{name="",
						   tag=none,fmt=fixed,
						   params=#fmt_params{content="AN",len=42},
						   %% decoder=fun(VAL)->
						   %% 		   <<ActTxAmt:12/bytes,
						   %% 		     _NotUsed/binary>> = VAL,
						   %% 		   #{"ACT-TX-AMT" => ActTxAmt}
						   %% 	   end},
						   decomposition=[
								  {"ACT-TX-AMT",12},
								  {filler,30}
								 ]}}
				     ]},
      96 => #iso_field_def{id=96,name="Message Security Code",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=16},variants=none},
      97 => #iso_field_def{id=97,name="Net Settlement Amount",
			   tag=none,fmt=fixed,params=#fmt_params{content="AN",len=16},variants=none},
      98 => #iso_field_def{id=98,name="Payee",
			   tag=none,fmt=fixed,params=#fmt_params{content="ANS",len=25},variants=none},
      99 => #iso_field_def{id=99,name="Settlement Institution Identification Code",
			   tag=none,fmt=llvar,params=#fmt_params{content="N",len=11},variants=none},
      100 => #iso_field_def{id=100,name="Receiving Institution Identification Code",
			    tag=none,fmt=llvar,params=#fmt_params{content="N",len=11},variants=none},
      101 => #iso_field_def{id=101,name="File Name",
			    tag=none,fmt=fixed,params=#fmt_params{content="ANS",len=4},variants=none},
      102 => #iso_field_def{id=102,name="Account Identification 1",
			    tag=none,fmt=llvar,params=#fmt_params{content="ANS",len=28},variants=none},
      103 => #iso_field_def{id=103,name="Account Identification 2",
			    tag=none,fmt=llvar,params=#fmt_params{content="ANS",len=28},variants=none},
      104 => #iso_field_def{id=104,name="Transaction Description",
			    tag=none,fmt=fixed,params=#fmt_params{content="ANS",len=63},variants=none},%% wliczony prefix dlugosci == 3
      105 => #iso_field_def{id=105,name="Reserved 105",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      106 => #iso_field_def{id=106,name="Reserved 106",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      107 => #iso_field_def{id=107,name="Reserved 107",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      108 => #iso_field_def{id=108,name="Reserved 108",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      109 => #iso_field_def{id=109,name="Reserved 109",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      110 => #iso_field_def{id=110,name="Reserved 110",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      111 => #iso_field_def{id=111,name="Reserved 111",
			    tag=none,fmt=lllvar,params=#fmt_params{content="ANS",len=999},variants=none},
      112 => #iso_field_def{id=112,name="Rozne...",tag=none,fmt=none,params=none,
			    variants=[
				      {fhm,#variant{name="Enhanced Preauthorized Hold Info",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="AN",len=105}}},%% wliczony prefix dlugosci == 3
				      {teller,#variant{name="Override Token",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="AN",len=157}}}%% wliczony prefix dlugosci == 3
				     ]},
      113 => #iso_field_def{id=113,name="Rozne...",
			    tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Bulk Check Amount Token",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=245}}},%% wliczony prefix dlugosci == 3
				      {fhm,#variant{name="Automated Hot Card Update",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="AN",len=276}}}%% wliczony prefix dlugosci == 3
				     ]},
      114 => #iso_field_def{id=114,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Bulk Check MICR 1",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=645}}},%% wliczony prefix dlugosci == 3
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="AN",len=429}}},%% wliczony prefix dlugosci == 3, UWAGA: tu jest tez alternatywne formatowanie ANS..276 z wliczonym prefixem 3...
				      {teller,#variant{name="WHFF Inquiry Token 1",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="AN",len=429}}}%% wliczony prefix dlugosci == 3
				     ]},
      115 => #iso_field_def{id=115,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Bulk Check MICR 2",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=643}}},%% wliczony prefix dlugosci == 3
				      {fhm,#variant{name="FHM CAF and PBF User Information",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=153}}},%% wliczony prefix dlugosci == 3
				      {teller,#variant{name="WHFF Inquiry Token Part 2",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="ANS",len=389}}}%% wliczony prefix dlugosci == 3
				     ]},
      116 => #iso_field_def{id=116,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Bulk Check SSBC Token Data",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=365}}},%% wliczony prefix dlugosci == 3
				      {fhm,#variant{name="FHM CAF non-currency dispense",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=155}}},%% wliczony prefix dlugosci == 3
				      {teller,#variant{name="WHFF Inquiry Token Part 3",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="ANS",len=389}}}%% wliczony prefix dlugosci == 3
				     ]},
      117 => #iso_field_def{id=117,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Bulk Check Disposition Token Data",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=25}}},%% wliczony prefix dlugosci == 3
				      {fhm,#variant{name="FHM CAF EMV",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=34}}},%% wliczony prefix dlugosci == 3
				      {teller,#variant{name="PBF Update TOken",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="ANS",len=7}}}%% wliczony prefix dlugosci == 3
				     ]},
      118 => #iso_field_def{id=118,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {fhm,#variant{name="FHM CAF and PBF",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=52}}},%% wliczony prefix dlugosci == 3, UWAGA: jest tez alternatywane formatowanie PBF: ANS 71
				      {teller,#variant{name="SPF Update TOken",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="ANS",len=103}}}%% wliczony prefix dlugosci == 3
				     ]},
      119 => #iso_field_def{id=119,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {fhm,#variant{name="FHM Self-Service Banking Check Information",
						    tag=none,fmt=fixed,
						    params=#fmt_params{content="ANS",len=67}}},%% wliczony prefix dlugosci == 3, UWAGA: jest tez alternatywane formatowanie NEG: ANS 13
				      {teller,#variant{name="WHFF Update TOken",
						       tag=none,fmt=fixed,
						       params=#fmt_params{content="ANS",len=83}}}%% wliczony prefix dlugosci == 3
				     ]},
      120 => #iso_field_def{id=120,name="Rozne...",
			    tag=none,fmt=none,params=none,   
			    variants=[
				      {netmgt,#variant{name="Key Mgt",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=9}}},
				      {atm,#variant{name="Terminal Address-Branch-Region",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=36}}},
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=53}}},
				      %% Uwaga: tu sa tez alternatywne formaty:
				      %% ANS 112
				      %% ANS 40
				      %% ANS 21
				      %% ANS 37
				      %% ANS 72
				      %% ANS 88
				      %% ANS 18
				      %% ANS 117
				      %% ANS 74
				      %% ANS 45
				      {pos,#variant{name="Terminal Address-Branch",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=36}}}, 
				      %% Uwaga: oryginalnie len=32, ale nie zgadzalo sie z przykladem z B24 więc ustawiam tak, jak dla ATM
				      {teller,#variant{name="Administrative Token",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=153}}}
				     ]},
      
      121 => #iso_field_def{id=121,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=79}}},%% Jest takze ANS 69
				      {pos,#variant{name="Authorization Indicators",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=23}}},
				      {telebanking,#variant{name="PIN Change Data",
							    tag=none,fmt=lllvar,
							    params=#fmt_params{content="ANS",len=35}}},
				      {teller,#variant{name="Native Message Token",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=153}}}
				     ]},
      
      
      %%----------------------------------------------------------
      %% Nie wypelnione mapowanie 122-122 !!!
      %%----------------------------------------------------------
      
      123 => #iso_field_def{id=123,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {netmgt,#variant{name="Cryptographic Service Message",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=553}}},
				      {atm,#variant{name="Deposit Credit Amount",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=15}}},
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=226}}},
				      %% Jest takze:
				      %% ANS 107
				      %% ANS 309
				      %% ANS 116
				      %%ANS 375
				      {pos,#variant{name="Invoice Data Record 1",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=23}}}, %% Jest tez ANS 171 
				      {telebanking,#variant{name="Account Qualifiers",
							    tag=none,fmt=lllvar,
							    params=#fmt_params{content="ANS",len=11}}},
				      {teller,#variant{name="SPF Inquiry Token",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=513}}}
				     ]},
      
      124 => #iso_field_def{id=124,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Depository Type",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=4}}},
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=663}}},
				      %% Jest takze:
				      %% ANS 244
				      %% ANS 620
				      %% ANS 453
				      {pos,#variant{name="Batch Shift Data",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=12}}}, %% Jest tez ANS 687 
				      {telebanking,#variant{name="Additional Data",
							    tag=none,fmt=lllvar,
							    params=#fmt_params{content="ANS",len=403}}},
				      {teller,#variant{name="Additional Data",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=687}}}
				     ]},
      
      125 => #iso_field_def{id=125,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Account Indicator",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=4}}},%% Jest tez ANS 375
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=135}}},
				      %% Jest takze:
				      %% ANS 13
				      %% ANS 276
				      {pos,#variant{name="Settlement Data 3",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=15}}}, %% Jest tez ANS 267 
				      {telebanking,#variant{name="Backup Account Info",
							    tag=none,fmt=lllvar,
							    params=#fmt_params{content="ANS",len=60}}}
				     ]},
      
      126 => #iso_field_def{id=126,name="Rozne...",tag=none,fmt=none,params=none,   
			    variants=[
				      {atm,#variant{name="Additional Data",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=800}}},
				      {fhm,#variant{name="Application File and Table Info",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=693}}},%% Uwaga, tu sa tez formatowania:
				      %% ANS 24
				      {pos,#variant{name="Preauthorization and chargeback data",
						    tag=none,fmt=lllvar,
						    params=#fmt_params{content="ANS",len=800}}}, %% Uwaga: oryginalnie len=41 ale nie zgadza sie z przykladem z B24 wiec ustawiam tak jak dla ATM
				      {telebanking,#variant{name="Inquiry Data",
							    tag=none,fmt=lllvar,
							    params=#fmt_params{content="ANS",len=791}}},%% UWAGA, tu jest wiele innych formatowan:
				      %% 994, 528, 863, 907, 819, 849, 759, 1001 ...
				      {teller,#variant{name="Inquiry Token",
						       tag=none,fmt=lllvar,
						       params=#fmt_params{content="ANS",len=673}}}
				     ]}
      
      %%----------------------------------------------------------
      %% Nie wypelnione mapowanie 127-128 !!!
      %%----------------------------------------------------------
      
     }.

%% BIC-ISO specific. Provided in "Token-ASCII Format"
-spec get_token_config() -> #{string() => #iso_token_def{}}.
get_token_config()->
    #{
      "B2" => #iso_token_def{tag="B2",name="EMV Request Data Token",
			     decomposition=[
					    {"BTMP",4},
					    {"USER-FLD1",4},
					    {"CRYPTO-INFO-DATA",2},
					    {"TVR",10},
					    {"ARQC",16},
					    {"AMT-AUTH",12},
					    {"AMT-OTHER",12},
					    {"AIP",4},
					    {"ATC",4},
					    {"TERM-CNTRY-CDE",3},
					    {"TRAN-CRNCY-CDE",3},
					    {"TRAN-DAT",6},
					    {"TRAN-TYPE",2},
					    {"UNPREDICT-NUM",8},
					    {"ISS-APPL-DATA-LGTH",4},
					    {"ISS-APPL-DATA",64}
					   ]},
      "B3" => #iso_token_def{tag="B3",name="EMV Discretionary Data Token",
			     decomposition=[
					    {"BTMP",4},
					    {"TERM-SERL-NUM",8},
					    {"EMV-TERM-CAP",8},
					    {"USER-FLD1",4},
					    {"USER-FLD2",8},
					    {"EMV-TERM-TYPE",2},
					    {"APPL-VER-NUM",4},
					    {"CVM-RSLTS",6},
					    {"DF-NAME-LGTH",4},
					    {"DF-NAME",32}
					   ]},
      "B4" => #iso_token_def{tag="B4",name="EMV Status Token",
			     decomposition=[
					    {"PT-SRV-ENTRY-MDE",3},
					    {"TERM-ENTRY-CAP",1},
					    {"LAST-EMV-STAT",1},
					    {"DATA-SUSPECT",1},
					    {"APPL-PAN-SEQ-NUM",2},
					    {"CVM-RSLTS",6},
					    %% positions 9-14 can have different meanings...
					    %% CAM-FLAGS
					    %% DEV-INFO
					    %% ICHG-DEF
					    %% or APPRVD-RC(2) + UNUSED(4)
					    {"RSN-ONL-CDE",4},
					    {"ARQC-VRFY",1},
					    {"ISO-RC-IND",1}
					   ]},
      "04" => #iso_token_def{tag="04",name="BASE24-pos Release 5.0 Token",
			     decomposition=[
					    {"ERR-FLG",1},
					    {"RTE-GRP",11},
					    {"CRD-VRFY-FLG",1},
					    {"CITY-EXT",5},
					    {"COMPLETE-TRC2-DATA",1},
					    {"UAF-FLG",1}
					   ]},
      "20" => #iso_token_def{tag="20",name="Interchange Compliance Token",
			     decomposition=[
					    {"LIFE-CYCLE-IND",1},
					    {"TRACE-ID",15},
					    {"VALID-CDE",4},
					    {"MONITORING-STAT",1},
					    {"ERR-IND",1}
					   ]}

     }.				     
