-module(yams_mqtt_lib).

%% API exports
-export([decode_str/1]).

%%====================================================================
%% API functions
%%====================================================================
decode_str(Bin) ->
    get_str(get_len(Bin)).
%%====================================================================
%% Internal functions
%%====================================================================
get_len(<<L:16, RestBin/binary>>) ->
    {ok, L, RestBin};
get_len(Bin) ->
    {error, invalid_input, Bin}.

get_str({ok, L, RestBin}) 
  when (size(RestBin) >= L) ->
    {BinOfInterest, RBin} = split_binary(RestBin, L),
    {ok, binary_to_list(BinOfInterest), RBin}; %% converting binary to list - is equivalent of converting it to string.
get_str({_, _, Bin}) ->
    {error, invalid_input, Bin}.
