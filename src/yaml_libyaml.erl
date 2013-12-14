%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%
%% @doc Wrapper for libyaml NIF.
%% @private

-module(yaml_libyaml).
-on_load(nif_init/0).

-export([binary_to_libyaml_event_stream/1]).
-export_type([event/0]).


%% miscellaneous libyaml structs

-type version_directive() ::
        null | {MajorNumber :: integer(), MinorNumber :: integer()}.

-type tag_directive() :: {TagHandle :: binary(), TagPrefix :: binary()}.

-type mark() :: {Index  :: non_neg_integer(),
                 Line   :: non_neg_integer(),
                 Column :: non_neg_integer()}.

%% miscellaneous supporting types

-type optional_string() :: null | binary().

%% libyaml enum types

-type encoding() :: any | utf8 | utf16le | utf16be.
%% -type break()      :: any |cr |ln | crln.
-type error_type() :: no | memory | reader | scanner | parser
                    | composer | writer | emitter.
-type scalar_style()   :: any | plain
                        | single_quoted | double_quoted
                        | literal | folded.
-type sequence_style() :: any | block | flow.
-type mapping_style()  :: any | block | flow.


%% yaml event types

-type stream_start() :: encoding().
-type document_start() :: {version_directive(),
                           [tag_directive()],
                           IsImplicit :: boolean()}.
-type document_end() :: IsImplicit :: boolean().
-type alias() :: binary().  %% anchor.
-type scalar() :: {Anchor :: optional_string(),
                   Tag    :: optional_string(),
                   Value  :: binary(),
                   %% boolean(), % Is the tag optional for any plain style?
                   %% boolean(), % Is the tag optional for any non-plain style?
                   scalar_style()}.

-type sequence_start() :: {Anchor :: optional_string(),
                           Tag    :: optional_string(),
                           %% IsOptional :: bolean()
                           sequence_style()}.

-type mapping_start() :: {Anchor :: optional_string(),
                          Tag    :: optional_string(),
                          %% IsOptional :: bolean()
                          mapping_style()}.

-type event2(Type, Body) :: {Type, Body, Start :: mark(), End :: mark()}.
-type event1(Type) :: event2(Type, null).

-type event() :: event2(stream_start, stream_start())
               | event1(stream_end)
               | event2(document_start, document_start())
               | event2(document_end, document_end())
               | event2(alias, alias())
               | event2(scalar, scalar())
               | event2(sequence_start, sequence_start())
               | event1(sequence_end)
               | event2(mapping_start, mapping_start())
               | event1(mapping_end).

%% @private
%% @doc Searches for NIF in private directory of "yamler" application.
%% @end
-spec nif_init() -> ok | {error, atom()}.
nif_init() ->
    PrivDir = case code:priv_dir(yamler) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

%% @private
%% @doc Helper for exiting gracefully when NIF can't be loaded.
%% @end
-spec not_loaded(pos_integer()) -> ok.
not_loaded(Line) -> exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

-spec binary_to_libyaml_event_stream(binary()) -> {ok, [event()]}
                                                | {error, {error_type(), binary()}}.
binary_to_libyaml_event_stream(Bin) when is_binary(Bin) ->
    case binary_to_libyaml_event_stream_rev(Bin) of
        {ok, List} -> {ok, lists:reverse(List)};
        {error, _Reason}=Error -> Error
    end.

binary_to_libyaml_event_stream_rev(_Bin) ->
	not_loaded(?LINE).
