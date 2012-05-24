%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%
%% @doc Public API for yaml application.

-module(yaml).

-export([load/1, load/2, load_file/1, load_file/2]).

%% @equiv load(YamlStream, [])
load(YamlStream) -> load(YamlStream, []).

%% @doc Convert a yaml binary to a list of documents.
%% Options:
%%
%% {schema, SchemaModule} - Select schema module.  Default is yaml_schema_erlang.
%%
%% implicit_atoms - For scalar values that look atom-ish, construct as atoms. (yaml_schema_erlang only)
%%
-spec load(binary(), proplist:proplist()) -> {ok, [yaml_compose:doc()]} | {error, Reason::term()}.
load(YamlStream, Opts) ->
	case yaml_libyaml:binary_to_libyaml_event_stream(YamlStream) of
		{ok, Events} ->
			yaml_compose:compose(Events, Opts);
		Else -> Else
	end.

%% @equiv load_file(Filename, [])
load_file(Filename) -> load_file(Filename, []).


%% @doc Load a yaml file from disk and convert to a list of documents.
%% See {@link yaml:load/2} for options.
-spec load_file(file:name(), proplists:proplist()) -> {ok, [yaml_compose:doc()]} | {error, Reason::term()}.
load_file(Filename, Opts) ->
	case file:read_file(Filename) of
		{ok,YamlStream} ->
			load(YamlStream, Opts);
		Else -> Else
	end.
