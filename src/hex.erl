%%============================================================================================
%% ISO8583 / BIC ISO Parser Hex Utils
%%--------------------------------------------------------------------------------------------
%% Artur Stefanowicz (C)
%% 2018-2020
%%============================================================================================
-module(hex).
-export([bin_to_hex/1,hex_to_bin/1]).

bin_to_hex(B) ->
	lists:flatten([io_lib:format("~2.16.0B",[X]) || X <- binary_to_list(B)]).

hex_to_bin(S) ->
	hex_to_bin(S,[]).
hex_to_bin([],Acc) ->
	list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T],Acc) ->
	{ok,[V],[]} = io_lib:fread("~16u",[X,Y]),
	hex_to_bin(T,[V|Acc]);
hex_to_bin([X|T],Acc) ->
	{ok,[V],[]} = io_lib:fread("~16u",lists:flatten([X,"0"])),
	hex_to_bin(T,[V|Acc]).
