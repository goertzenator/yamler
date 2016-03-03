%%
%% @author Daniel Goertzen <daniel.goertzen@gmail.com>
%% @copyright 2012 Daniel Goertzen
%% %@license See file /LICENSE
%%


-module(yaml_tests).

-export([generate_results/0]).

-include_lib("eunit/include/eunit.hrl").

pairs() -> [
			% in, out, Opts
			{"test_failsafe", "test_failsafe", [{schema, yaml_schema_failsafe}]},
			{"test_json", "test_json", [{schema, yaml_schema_json}]},
			{"test_core", "test_core", [{schema, yaml_schema_core}]},
			{"test_erlang", "test_erlang1", [{schema, yaml_schema_erlang}]},
			{"test_erlang", "test_erlang2", [{schema, yaml_schema_erlang}, implicit_atoms]},
			{"test_erlang", "test_erlang1", []},
			{"test_erlang", "test_erlang2", [implicit_atoms]}
		   ].


yaml_filename(File) ->
	filename:join(
	  [
	   code:lib_dir(yamler, test),
	   "yaml",
	   File++".yaml"]).

output_term_filename(File) ->
	filename:join(
	  [
	   code:lib_dir(yamler, test),
	   "outputs",
	   File++".term"]).

generate_results() ->
	lists:foreach(
	  fun({In, Out, Opts}) ->
			  Term = yaml:load_file(yaml_filename(In), Opts),
			  Txt = iolist_to_binary(io_lib:format("~p.~n",[Term])),
			  %io:format("writing ~p~n~p~n",[output_term_filename(Out), Txt]),
			  file:write_file(output_term_filename(Out), Txt)
	  end,
	  pairs()).

pairs_test_() ->
	io:format("processing pairs ~p~n", [pairs()]),
	lists:map(fun({In, Out, Opts}) ->
					  %?debugFmt("~p\n\n", [yaml:load_file(yaml_filename(In), Opts)]),
					  ?_assertEqual(

					  begin
					  	{ok,[Term]} = file:consult(output_term_filename(Out)),
						Term
					  end,

					  yaml:load_file(yaml_filename(In), Opts)

					  )
			  end,
			  pairs()).




