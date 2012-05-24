# goyaml, an Erlang YAML Loader

This application loads [YAML](http://en.wikipedia.org/wiki/Yaml) files into Erlang.  This implementation supports:

* Detailed errors on yaml load failures (line, column, reason)
* Anchors and aliases
* [Merge tags](http://yaml.org/type/merge.html)
* The tag `!atom` for explicitely tagging values as atoms.
* An `implicit_atoms` mode to interpret values that look atom-ish as atoms.
* Customizable schemas via callback modules.

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

	{ok,[[{customer,[{family,<<"Gale">>},{given,<<"Dorothy">>}]},
	      {items,[[{descrip,<<"Water Bucket (Filled)">>},
	               {price,1.47},
	               {quantity,4},
	               {part_no,<<"A4786">>}],
	              [{descrip,<<"High Heeled \"Ruby\" Slippers">>},
	               {price,100.27},
	               {size,8},
	               {quantity,1},
	               {part_no,<<"E1628">>}]]},
	      {receipt,<<"Oz-Ware Purchase Invoice">>},
	      {date,<<"2007-08-06">>},
	      {specialDelivery,<<"Follow the Yellow Brick Road to the Emerald City. Pay no attention to the ma"...>>},
	      {ship_to,[{street,<<"123 Tornado Alley\nSuite 16\n">>},
	                {state,<<"KS">>},
	                {city,<<"East Centerville">>}]},
	      {bill_to,[{street,<<"123 Tornado Alley\nSuite 16\n">>},
	                {state,<<"KS">>},
	                {city,<<"East Centerville">>}]}]]}


# Installation

### Download
Download the code...

	$ git clone git://github.com/goertzenator/goyaml.git

### Compile
Build the source and documentation and run tests with...

	$ cd goyaml
	$ rebar compile doc eunit

The NIF module spews a few warnings.  This is okay.

Play with it..

	$ export ERL_LIBS=$(pwd)
	$ erl
	Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:3:3] [async-threads:0] [kernel-poll:false]
	
	Eshell V5.9.1  (abort with ^G)
	1> yaml:load_file("test/yaml/demo1.yaml").
	{ok,[[{<<"specialDelivery">>,
	       <<"Follow the Yellow Brick Road to the Emerald City. Pay no attention to the man 	behind the cur"...>>},
	      {<<"items">>, ...



# Schemas

Tag resolution and value construction are implemented in *schema callback modules*.  This application includes 4 different schema callback modules, and others can be implemented if different behavior is required.

The included schemas are:

* `yaml_schema_failsafe`
* `yaml_schema_json`
* `yaml_schema_core`
* `yaml_schame_erlang` (default)

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

# Limitations

* This app does not emit yaml.
* This app may stumble on enormous yaml files because the entire file, event stream, and constructed documents must fit in memory at once.  Also, a scheduler may be stuck in a NIF call for a while when parsing a very large yaml file.


