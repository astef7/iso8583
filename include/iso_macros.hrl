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
%% ISO Common Marcos
%%==============================================================================
%% 2020-02-08 (wersja kontrolera gphs-c)
%%==============================================================================

-define(DEFAULT_LOGGING_HEX,false).

-ifdef(debug).
-define(LOG(S,V),logger:info(lists:append([?LOG_PFX," ",S]),V)).
-else.
-define(LOG(S,V),ok).
-endif.

-ifdef(debug).
-define(LOG_HEX(Fmt,Params,DataBin),
	case ?DEFAULT_LOGGING_HEX of
	    true ->
		io:format(lists:append([?LOG_PFX," ",Fmt]),
			  Params++[lists:flatten(io_lib:format("~s",[dump:binary(DataBin)]))]);
	    false ->
		ok
	end
       ).
-else.
-define(LOG_HEX(Fmt,Params,DataBin),ok).
-endif.

-ifdef(exometer).
-define(EXOM_GAUGE(Name,Value),exoutils:update_gauge(Name,Value)).
-else.
-define(EXOM_GAUGE(Name,Value),io:format("NO-EXO-UTILS",[])).
-endif.

-ifdef(exometer).
-define(EXOM_SPIRAL(Name,Value),exoutils:update_spiral(Name,Value)).
-else.
-define(EXOM_SPIRAL(Name,Value),io:format("NO-EXO-UTILS",[])).
-endif.

