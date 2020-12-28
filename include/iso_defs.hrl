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
%% ISO8583 / BIC ISO Parser Type Definitions
%%------------------------------------------------------------------------------
%% Artur Stefanowicz (C)
%% 2018-2020
%%==============================================================================
-type iso_fmt_type() :: fixed | llvar | lllvar.
-type iso_prod_type() :: pos | atm | teller | telebanking | netmgt | fhm.
-type iso_netmgt_evens_type() :: sign_on | sign_off | echo.

%% Parsed iso field record
-record(iso_field,{id::integer(),
		   tag::string(),
		   value::binary(),
		   decoder = none :: fun((VAL::binary())-> ITEMS::#{}) | none,
		   encoder = none :: fun((ITEMS::map())-> VAL::binary()) | none
		  }). 

%% Parsed iso token record
-record(iso_token,{tag = none ::string() | none,
		   decoder = none :: fun((VAL::binary())-> ITEMS::#{}) | none,
		   encoder = none :: fun((ITEMS::map())-> VAL::binary()) | none,
		   value::binary()
		  }).

%% Field format params
-record(fmt_params,{content::string(),
		    len::integer()}).

%% Field definition variant
-record(variant,{name::string(),
		 tag::string() | none,
		 fmt::iso_fmt_type(),
		 params::#fmt_params{},
		 decomposition = none :: list({TAG::string() | filler,LEN::integer()}) | none,
		 decoder = none ::fun((VAL::binary())-> ITEMS::map()) | none,
		 encoder = none ::fun((ITEMS::map())-> VAL::binary()) | none}).

%% Iso field definition
-record(iso_field_def,{id::integer(),
		       name::string(),
		       tag::string() | none,
		       fmt::iso_fmt_type() | none,
		       params::#fmt_params{} | none,
		       decomposition = none :: list({TAG::string() | filler,LEN::integer()}) | none,
		       decoder = none :: fun((VAL::binary())-> ITEMS::map()) | none,
		       encoder = none :: fun((ITEMS::map())-> VAL::binary()) | none,
		       variants::[{iso_prod_type(),#variant{}}] | none}).

%% Iso token definition
-record(iso_token_def,{tag::string(),
		       name = none ::string() | none,
		       decomposition = none ::list({TAG::string() | filler,LEN::integer()}) | none}).

%% Parsed message structure
-record(iso_msg,{prod::iso_prod_type(),
		 mti::string(),
		 fields = #{} :: #{ID::integer() := #iso_field{}},
		 tokens = #{} :: #{TAG::string() := #iso_token{}},
		 tags = #{} :: #{TAG::string() := binary()},
		 org = none :: MSG::binary() | none}).
