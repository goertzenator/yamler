%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%
%% @doc Wrapper for libyaml NIF.
%% @private

-module(yaml_libyaml).

-export([binary_to_libyaml_event_stream/1]).
-export_type([event/0]).

-on_load(init/0).


%% miscellaneous libyaml structs

-type(version_directive() ::
    null |
    {integer(), % The major version number.
    integer() % The minor version number.
    }).
    
-type(tag_directive() ::
    {binary(), % The tag handle.
    binary() % The tag prefix.
    }).

-type(mark() :: 
    {non_neg_integer(), % The position index.
    non_neg_integer(), % The position line.
    non_neg_integer()  % The position column.
    }).

    
%% miscellaneous supporting types

-type(optional_string() :: null | binary()).    


%% libyaml enum types

-type(encoding() :: any|utf8|utf16le|utf16be).
%-type(break() :: any|cr|ln|crln).
-type(error_type() :: no|memory|reader|scanner|parser|composer|writer|emitter).
-type(scalar_style() :: any|plain|single_quoted|double_quoted|literal|folded).
-type(sequence_style() :: any|block|flow).
-type(mapping_style() :: any|block|flow).

    



%% yaml event types

-type(stream_start() ::
    encoding() % The document encoding.
    ).

-type(document_start() ::
    {version_directive(),    % The version directive.
    [tag_directive()],  % The list of tag directives
     boolean() % Is the document indicator implicit?
     }).

-type(document_end() ::
     boolean() % Is the document end indicator implicit?
     ).

-type(alias() ::
    binary() % The anchor.
    ).
    
-type(scalar() ::
    {optional_string(), % The anchor.
    optional_string(), % The tag.
    binary(), % The scalar value.
%    boolean(), % Is the tag optional for any plain style?
%    boolean(), % Is the tag optional for any non-plain style?
    scalar_style() % The scalar style.
    }).
    
-type(sequence_start() ::
    {optional_string(), % The anchor.
    optional_string(), % The tag.
%    boolean(), % Is the tag optional?
    sequence_style() % The sequence style.
     }).
     
-type(mapping_start() ::
    {optional_string(), % The anchor.
    optional_string(), % The tag.
%    boolean(), % Is the tag optional?
    mapping_style() % The mapping style.
     }).
     
-type(event2(Event_type, Event_body) ::
    {Event_type, Event_body,
    mark(), % The beginning of the event.
    mark() % The end of the event.
    }).

-type(event1(Event_type) :: event2(Event_type, null)).

    
-type(event() ::
    event2(stream_start, stream_start()) |
    event1(stream_end) |
    event2(document_start, document_start()) |
    event2(document_end, document_end()) |
    event2(alias, alias()) |
    event2(scalar, scalar()) |
    event2(sequence_start, sequence_start()) |
    event1(sequence_end) |
    event2(mapping_start, mapping_start()) |
    event1(mapping_end)).
        

init() ->
	PrivDir = code:priv_dir(yamler),
	Lib = filename:join([PrivDir, "libyaml"]),
	ok = erlang:load_nif(Lib, 0).

-spec binary_to_libyaml_event_stream(binary()) -> {ok,[event()]} | {error,{error_type(),binary()}}.
binary_to_libyaml_event_stream(Bin) ->
    case binary_to_libyaml_event_stream_rev(Bin) of
        {ok, List} -> {ok, lists:reverse(List)};
        Else -> Else
    end.

binary_to_libyaml_event_stream_rev(_) ->
	erlang:nif_error(nif_library_not_loaded).


