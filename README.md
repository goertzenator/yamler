# yamler, an Erlang YAML Loader

This application loads [YAML](http://en.wikipedia.org/wiki/Yaml) files into Erlang.  This implementation supports:

* Detailed errors on yaml load failures (line, column, reason)
* Anchors and aliases
* [Merge tags](http://yaml.org/type/merge.html)
* The tag `!atom` for explicitely tagging values as atoms.
* An `implicit_atoms` mode to interpret values that look atom-ish as atoms.
* Customizable schemas via callback modules.
* Mappings returned as native Erlang R17 maps. (passes all unit tests on experimental maps branch, edoc currently broken)

This application embeds the C yaml parser "[libyaml](http://pyyaml.org/wiki/LibYAML)" which is compiled as a NIF.
 
# Example

###The yaml file...

    # demo1.yaml
    ---
	receipt:     Oz-Ware Purchase Invoice
	date:        2007-08-06
	customer:
	    given:   Dorothy
	    family:  Gale
	
	items:
	    - part_no:   A4786
	      descrip:   Water Bucket (Filled)
	      price:     1.47
	      quantity:  4
	
	    - part_no:   E1628
	      descrip:   High Heeled "Ruby" Slippers
	      size:      8
	      price:     100.27
	      quantity:  1
	
	bill_to:  &id001
	    street: |
	            123 Tornado Alley
	            Suite 16
	    city:   East Centerville
	    state:  KS
	
	ship_to:  *id001
	
	specialDelivery:  >
	    Follow the Yellow Brick
	    Road to the Emerald City.
	    Pay no attention to the
	    man behind the curtain.
	...



###Load command...

	3> yaml:load_file("test/yaml/demo1.yaml", [implicit_atoms]).

    {ok,[#{bill_to => #{city => <<"East Centerville">>,
             state => <<"KS">>,
             street => <<"123 Tornado Alley\nSuite 16\n">>},
           customer => #{family => <<"Gale">>,given => <<"Dorothy">>},
           date => <<"2007-08-06">>,
           items => [#{descrip => <<"Water Bucket (Filled)">>,
              part_no => <<"A4786">>,
              price => 1.47,
              quantity => 4},
            #{descrip => <<"High Heeled \"Ruby\" Slippers">>,
              part_no => <<"E1628">>,
              price => 100.27,
              quantity => 1,
              size => 8}],
           receipt => <<"Oz-Ware Purchase Invoice">>,
           ship_to => #{city => <<"East Centerville">>,
             state => <<"KS">>,
             street => <<"123 Tornado Alley\nSuite 16\n">>},
           specialDelivery => <<"Follow the Yellow Brick Road to the Emerald City. Pay no attention to the ma"...>>}]}


# Installation

### Download
Download the code...

	$ git clone git://github.com/goertzenator/yamler.git

### Compile
Build the source and documentation and run tests with...

	$ cd yamler
	$ rebar compile doc eunit

The NIF module spews a few warnings.  This is okay.

Play with it..

	$ export ERL_LIBS=$(pwd)
	$ erl
    Erlang R17A (erts-5.11) [source-16b4dc1] [64-bit] [smp:3:3] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V5.11  (abort with ^G)
    1> yaml:load_file("test/yaml/demo1.yaml").
    {ok,[#{<<"bill_to">> => #{<<"city">> => <<"East Centerville">>,
             <<"state">> => <<"KS">>, ...


# Schemas

Tag resolution and value construction are implemented in *schema callback modules*.  This application includes 4 different schema callback modules, and others can be implemented if different behavior is required.

The included schemas are:

* `yaml_schema_failsafe`
* `yaml_schema_json`
* `yaml_schema_core`
* `yaml_schema_erlang` (default)

The schema is selected with the `schema` option, for example:

	4> yaml:load_file("test/yaml/demo1.yaml", [{schema, yaml_schema_core}]).
	{ok,[[{<<"specialDelivery">>, ...



`failsafe` and `json` are defined by the yaml spec.  They are of limited use, however the schema modules can be instructive if you want to write your own.

The `core` schema (defined by the yaml spec) has most of what you need to make good use of yaml.  The `erlang` schema extends core with [merge](http://yaml.org/type/merge.html) and atom support.

To explicitely tag a value as an atom, use the `!atom` tag, for example:

	- !atom an_atom
	- !atom yet another atom

To turn on implicit atom detection for untagged values, use the erlang schema and pass in the `implicit_atoms` options, for example:

	5> yaml:load_file("test/yaml/demo1.yaml", [implicit_atoms]). 

The following yaml fragment illustrates which values will be recognized as atoms:

	- "not an atom"
	- not an atom
	- 'an atom'
	- an_atom
	- anatom
	- aNA_t@OM@_
	- Not_an_atom
	- Notanatom
	- "not_an_atom"


## Custom Schemas

See the behavior documenation for yaml_schema and the 4 schemas included in this application.

# Tips

If you are using erlide, I recommend the eclipse yaml editor [yedit](http://code.google.com/p/yedit/).

Erlang R17 implements a native map type which is used by yamler to represent yaml mappings.  If you need to use an older Erlang see the git branch `mapping_as_list`.

# Limitations

* This app does not emit yaml.
* This app may stumble on enormous yaml files because the entire file, event stream, and constructed documents must fit in memory at once.  Also, a scheduler may be stuck in a NIF call for a while when parsing a very large yaml file.


