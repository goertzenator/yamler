%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%
%% @doc Compose and construct an event stream into output documents.
%% @private

-module(yaml_compose).


-export([compose/2]).
-export_type([doc/0]).

-type doc() :: ynode().
%% A yaml document.  Yaml files may contain many documents.

-type ynode() :: scalar() | mapping() | sequence().
%% Yaml node, a generic component of a yaml document.  (node() is a built-in type, so ynode() is used)

-type scalar() :: term().
%% Non-container element of yaml document.

-type mapping() :: [{ynode(), ynode()}].
%% Unordered associative list of nodes.

-type sequence() :: [ynode()].
%% Unordered sequence of nodes.



-record(state, {
				events :: [yaml_libyaml:event()],
				anchors :: dict:dict(),
				schema :: atom(),
				schema_state :: term()
			   }).
% composer state

%% throw_error.  Used to pass errors up to compose/3
throw_error(Msg,{_Event, _Data, _StartMark={_Pos, Line, Column}, _EndMark}) ->
	Txt = unicode:characters_to_list([Msg,
									  ", Line ",integer_to_list(Line+1),
									  ", Column ",integer_to_list(Column+1)]),
	throw({error, Txt}).


%% @doc Convert list of yaml events into list of yaml documents.
-spec compose([yaml_libyaml:event()], [term()]) -> {ok, [doc()]} | {error, term()}.
compose(Events, Opts) ->
	Schema = proplists:get_value(schema, Opts, yaml_schema_erlang),
	State = #state{
				   events = Events,
				   anchors = dict:new(),
				   schema = Schema,
				   schema_state = Schema:init(Opts)
				  },
	catch compose_stream(State).


compose_stream( State0 = #state{events=[{stream_start, _,_,_} | Rest]}) ->
	State1 = State0#state{events=Rest},
	Docs = compose_docs(State1),
	{ok, Docs}.

compose_docs(State) -> compose_docs([], State).
compose_docs(Docs, State0) ->
	case compose_doc(State0) of
		{ok, Doc, State1} -> compose_docs([Doc|Docs], State1);
		{stream_end, _State1} -> lists:reverse(Docs)
	end.



% empty document case
compose_doc(State0 = #state{events=[{document_start, _,_,_}, {document_end, _,_,_} | Rest]}) ->
	State1 = State0#state{events=Rest},
	{ok, null, State1};

% normal document case
compose_doc(State0 = #state{events=[{document_start, _,_,_} | Rest]}) ->
	State1 = State0#state{events=Rest},
	{{_ResolvedTag, ConstructedValue}, State2} = compose_node(State1),

	% remove document_end
	[{document_end, _,_,_} | Rest2] = State2#state.events,
	State3 = State2#state{events=Rest2},

	{ok, ConstructedValue, State3};

% stream end case
compose_doc(State0 = #state{events=[{stream_end, _,_,_}]}) ->
	State1 = State0#state{events=[]},
	{stream_end, State1}.


-spec compose_node(#state{}) -> {ynode(), #state{}}.
% handle scalar
compose_node(State0 = #state{
							 events=[Head={scalar, {Anchor,Tag,Value,Style},_,_} |Rest],
							 schema = Schema,
							 schema_state = SchemaState
							}) ->
	ResolvedTag = case Schema:resolve_scalar_tag(Tag, Value, Style, SchemaState) of
					  {ok, T} -> T;
					  nomatch -> throw_error("Unknown tag", Head)
				  end,
	ConstructedValue = case Schema:construct_scalar(ResolvedTag, Value, SchemaState) of
						   {ok, V} -> V;
						   _ -> throw_error("Cannot construct scalar", Head)
					   end,
	State1 = State0#state{events=Rest},
	Node = {ResolvedTag, ConstructedValue},
	{Node, maybe_anchor(Anchor, Node, Head, State1)};

% handle alias
compose_node(State0 = #state{
							 events=[Head={alias, Anchor,_,_} |Rest],
							 anchors = Anchors}) ->
	case dict:find(Anchor, Anchors) of
		{ok, Node} ->
			State1 = State0#state{events=Rest},
			{Node, State1};
		error ->
			throw_error("Alias not defined", Head)
	end;

% handle sequence
compose_node(State0 = #state{
							 events = [Head={sequence_start, {Anchor,Tag,_Style},_,_} |Rest],
							 schema = Schema,
							 schema_state = SchemaState}) ->
	State1 = State0#state{events=Rest},
	{Nodes, State2} = compose_sequence([], State1),
	ResolvedTag = case Schema:resolve_sequence_tag(Tag, Nodes, SchemaState) of
					  {ok, T} -> T;
					  nomatch -> throw_error("Unknown tag", Head)
				  end,
	ConstructedValue = case Schema:construct_sequence(ResolvedTag, Nodes, SchemaState) of
						   {ok, V} -> V;
						   _ -> throw_error("Cannot construct sequence", State0)
					   end,
	Node = {ResolvedTag, ConstructedValue},
	{Node, maybe_anchor(Anchor, Node, Head, State2)};

% handle mapping
compose_node(State0 = #state{
							 events=[Head={mapping_start, {Anchor,Tag,_Style},_,_} |Rest],
							 schema = Schema,
							 schema_state = SchemaState}) ->
	State1 = State0#state{events=Rest},
	{Nodes, State2} = compose_mapping(State1),
	ResolvedTag = case Schema:resolve_mapping_tag(Tag, Nodes, SchemaState) of
					  {ok, T} -> T;
					  nomatch -> throw_error("Unknown tag", Head)
				  end,
	ConstructedValue = case Schema:construct_mapping(ResolvedTag, Nodes, SchemaState) of
						   {ok, V} -> V;
						   _ -> throw_error("Cannot construct mapping", Head)
					   end,
	Node = {ResolvedTag, ConstructedValue},
	{Node, maybe_anchor(Anchor, Node, Head, State2)}.



-spec compose_sequence([ynode()], #state{}) -> {sequence(), #state{}}.
compose_sequence(ConstructedValues, State0 = #state{
													events=[{sequence_end, _,_,_} |Rest]}) ->
	State1 = State0#state{events=Rest},
	{lists:reverse(ConstructedValues), State1};
compose_sequence(ConstructedValues, State0) ->
	{Node, State1} = compose_node(State0),
	{_ResolvedTag, ConstructedValue} = Node,
	compose_sequence([ConstructedValue|ConstructedValues], State1).


-spec compose_mapping(#state{}) -> {mapping(), #state{}}.
compose_mapping(State) -> compose_mapping(#{}, #{}, State).

%% Accumulate plain old mapping entries in Map, and merge entries in Merge.
compose_mapping(Map0, Merge, State0 = #state{events=[{mapping_end, _,_,_} |Rest]}) ->
	State1 = State0#state{events=Rest},

	% notes from http://yaml.org/type/merge.html
	% - merged keys do not overwrite any other keys
	% - latter merged keys do not overwrite earlier merged keys
	{ maps:merge(Merge, Map0),  % keys in Map take precendence over Merge
	  State1 };

compose_mapping(Map0, Merge0, State0) ->
	{{ KTag, Key}, State1} = compose_node(State0),
	{{ VTag, Value}, State2} = compose_node(State1),

	case {KTag, VTag} of
		%% add single mapping to merge map
		{'tag:yaml.org,2002:merge', 'tag:yaml.org,2002:map'} ->
			Merge1 = maps:merge(Value, Merge0),
			compose_mapping(Map0, Merge1, State2);

		%% add list of mappings to merge map
		{'tag:yaml.org,2002:merge', 'tag:yaml.org,2002:seq'} ->
			Merge1 = case catch lists:foldl(fun maps:merge/2, Merge0, Value) of
						 badmap ->
							 throw_error("Merge sequence must contain only mappings", hd(State1#state.events));
						 Else when is_map(Else) ->
							 Else
					 end,
			compose_mapping(Map0, Merge1, State2);

		{'tag:yaml.org,2002:merge', _} ->
			throw_error("Merge value must be mapping or sequence of mappings", hd(State1#state.events));

		_ ->
			Map1 = case maps:is_key(Key, Map0) of
					   false -> maps:put(Key, Value, Map0);
					   true  -> throw_error("Duplicate key", hd(State0#state.events))
				   end,
			compose_mapping(Map1, Merge0, State2)
	end.

-spec maybe_anchor(Anchor, Value, Event, State) -> #state{}
when
  Anchor :: null | binary(),
  Value :: term(),
  Event :: yaml_libyaml:event(),
  State :: #state{}.

maybe_anchor(null, _Value, _Event, State0) -> State0;
maybe_anchor(Anchor, Value, Event, State0=#state{anchors=Anchors0}) ->
	case dict:is_key(Anchor, Anchors0) of
		true -> throw_error("Anchor already defined", Event);
		false ->
			State0#state{anchors = dict:store(Anchor, Value, Anchors0)}
	end.

