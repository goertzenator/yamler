%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%


-module(yaml_schema_core).

-export([init/1, destroy/1]).
-export([resolve_mapping_tag/3, resolve_sequence_tag/3, resolve_scalar_tag/4]).
-export([construct_mapping/3, construct_sequence/3, construct_scalar/3]).

%% Core Schema as per Yaml 1.2 section 10.3

-record(state, {
				tag_regexs
			   }).

% Regexes for inferring yaml tag when tag is not present
tag_regexs() ->
	[
	 {<<"^[-+]?[0-9]+$">>,            'tag:yaml.org,2002:int'},
	 {<<"^0o[0-7]+$">>,               'tag:yaml.org,2002:int'},
	 {<<"^0x[0-9a-fA-F]+$">>,         'tag:yaml.org,2002:int'},
	 {<<"^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$">>, 'tag:yaml.org,2002:float'},
	 {<<"^[-+]?(\\.inf|\\.Inf|\\.INF)$">>, 'tag:yaml.org,2002:float'},
	 {<<"^(\\.nan|\\.NaN|\\.NAN)$">>, 'tag:yaml.org,2002:float'}
	].

% Precompile regexes
init(_Opts) ->
	#state{
		   tag_regexs = [ begin
							  {ok, Compiled} = re:compile(Regex), 
							  {Compiled, Tag}
						  end || {Regex, Tag} <- tag_regexs() ] 
		  }.
destroy(_State) -> ok.

%% mapping
resolve_mapping_tag(Tag, Value, State) -> yaml_schema_failsafe:resolve_mapping_tag(Tag, Value, State).
construct_mapping(Tag, Value, State) ->yaml_schema_failsafe:construct_mapping(Tag, Value, State).

%% sequence
resolve_sequence_tag(Tag, Value, State) -> yaml_schema_failsafe:resolve_sequence_tag(Tag, Value, State).
construct_sequence(Tag, Value, State) ->yaml_schema_failsafe:construct_sequence(Tag, Value, State).

%% scalar
resolve_scalar_tag(null, <<"null">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:null'};
resolve_scalar_tag(null, <<"Null">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:null'};
resolve_scalar_tag(null, <<"NULL">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:null'};
resolve_scalar_tag(null, <<"~">>, plain, _State)     -> {ok, 'tag:yaml.org,2002:null'};

resolve_scalar_tag(null, <<"true">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(null, <<"True">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(null, <<"TRUE">>, plain, _State)  -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(null, <<"false">>, plain, _State) -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(null, <<"False">>, plain, _State) -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(null, <<"FALSE">>, plain, _State) -> {ok, 'tag:yaml.org,2002:bool'};

resolve_scalar_tag(null, Value, plain, _State = #state{tag_regexs = Regexs}) ->
    % drop non-matching regexes from list
    ResultList = lists:dropwhile(
        fun ({Rexp, _Tag}) ->
            case re:run(Value, Rexp, [{capture,none}]) of
                match -> false;
                nomatch -> true
            end
        end,
        Regexs),
    case ResultList of
        [{_, Tag}|_] -> {ok, Tag};
        [] -> {ok, 'tag:yaml.org,2002:str'}
    end;
    
% untagged, non-plain
resolve_scalar_tag(null, _Value, _Style, _State) ->{ok, 'tag:yaml.org,2002:str'};

resolve_scalar_tag(Tag, _Value, _Style, _State) -> resolve_scalar_tag(Tag).
resolve_scalar_tag(<<"!">>)                        -> {ok, 'tag:yaml.org,2002:str'};
resolve_scalar_tag(<<"tag:yaml.org,2002:null">>)   -> {ok, 'tag:yaml.org,2002:null'};
resolve_scalar_tag(<<"tag:yaml.org,2002:bool">>)   -> {ok, 'tag:yaml.org,2002:bool'};
resolve_scalar_tag(<<"tag:yaml.org,2002:int">>)    -> {ok, 'tag:yaml.org,2002:int'};
resolve_scalar_tag(<<"tag:yaml.org,2002:float">>)  -> {ok, 'tag:yaml.org,2002:float'};
resolve_scalar_tag(<<"tag:yaml.org,2002:str">>)    -> {ok, 'tag:yaml.org,2002:str'};
resolve_scalar_tag(_)                               -> nomatch.

construct_scalar(Tag, Value, _State) -> construct_scalar(Tag, Value).
construct_scalar('tag:yaml.org,2002:null', <<"null">>)  -> {ok, null};
construct_scalar('tag:yaml.org,2002:null', <<"Null">>)  -> {ok, null};
construct_scalar('tag:yaml.org,2002:null', <<"NULL">>)  -> {ok, null};
construct_scalar('tag:yaml.org,2002:null', <<"~">>)     -> {ok, null};

construct_scalar('tag:yaml.org,2002:bool', <<"true">>)  -> {ok, true};
construct_scalar('tag:yaml.org,2002:bool', <<"True">>)  -> {ok, true};
construct_scalar('tag:yaml.org,2002:bool', <<"TRUE">>)  -> {ok, true};
construct_scalar('tag:yaml.org,2002:bool', <<"false">>) -> {ok, false};
construct_scalar('tag:yaml.org,2002:bool', <<"False">>) -> {ok, false};
construct_scalar('tag:yaml.org,2002:bool', <<"FALSE">>) -> {ok, false};

construct_scalar('tag:yaml.org,2002:float', <<".nan">>) -> {ok, nan};
construct_scalar('tag:yaml.org,2002:float', <<".NaN">>) -> {ok, nan};
construct_scalar('tag:yaml.org,2002:float', <<".NAN">>) -> {ok, nan};

construct_scalar('tag:yaml.org,2002:int', Value) ->
    {Base, IntPart} = case Value of
        <<"0x",P/binary>> -> {16, P};
        <<"0o",P/binary>> -> {8, P};
        P                 -> {10, P}
    end,
    case catch list_to_integer(binary_to_list(IntPart), Base) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> badvalue
    end;

construct_scalar('tag:yaml.org,2002:float', Value) ->
    case string:to_float(binary_to_list(Value)) of
        {Num, []} -> {ok, Num};
        _ ->
            %% extract infinity sign
            {Inf, RValue} = case Value of
                <<"+", Rest/binary>> -> {inf, Rest};
                <<"-", Rest/binary>> -> {ninf, Rest};
                Rest -> {inf, Rest}
            end,
            case RValue of
                <<".inf">> -> {ok, Inf};
                <<".Inf">> -> {ok, Inf};
                <<".INF">> -> {ok, Inf};
                _ -> nomatch
            end
    end;
construct_scalar('tag:yaml.org,2002:str', Value) -> {ok, Value};

construct_scalar(_, _) -> nomatch.


    










