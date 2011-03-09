all:
	rebar compile

clean:
	rebar clean

test: all
	erl -noshell -noinput -pa ebin -s stree main kernel_sup -s init stop
	erl -noshell -noinput -pa ebin -s ptree main kernel_sup -s init stop
