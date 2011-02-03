all:
	erlc -o ebin src/*.erl

clean:
	rm -rf ebin/*.beam

test: all
	erl -noshell -noinput -pa ebin -s stree main kernel_sup -s init stop
