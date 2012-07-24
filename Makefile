.PHONY: all test console compile
all: test

test_clean: get-deps
	rm -rf tests/*.beam
	make test

cover_test_clean: get-deps
	rm -rf tests/*.beam
	make cover_test

test: prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/deps/escalus/ebin \
			`pwd`/deps/exml/ebin \
			`pwd`/deps/base16/ebin \
		-s run_common_test ct

cover_test: prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/deps/escalus/ebin \
			`pwd`/deps/exml/ebin \
			`pwd`/deps/base16/ebin \
		-s run_common_test ct_cover; \

cover_summary: prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/deps/escalus/ebin \
			`pwd`/deps/exml/ebin \
			`pwd`/deps/base16/ebin \
		-s run_common_test cover_summary; \

prepare: compile
	erlc -Ideps/exml/include \
		 run_common_test.erl
	mkdir -p ct_report
	cp files/sample_external_auth.py /tmp
	cp files/test_cert.pem /tmp/server.pem

console: compile
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/deps/escalus/ebin \
			`pwd`/deps/exml/ebin \
			`pwd`/deps/base16/ebin

compile: get-deps
	./rebar compile

get-deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
