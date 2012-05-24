%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%
%% @doc Schema behavior module.  See {@link yaml_schema_failsafe} for fully documented schema example.

-module(yaml_schema).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{init,1}, {destroy,1},
	 {resolve_mapping_tag,3}, {resolve_sequence_tag,3}, {resolve_scalar_tag,4},
	 {construct_mapping,3}, {construct_sequence,3}, {construct_scalar,3}];

behaviour_info(_) ->
	undefined.
