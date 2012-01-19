.PHONY: all test

all: test

test_clean: escalus/ebin
	rm -rf tests/*.beam
	make test

test:
	test -e escalus/Makefile || git submodule update --init --recursive
	cd escalus; make
	erlc -I escalus/deps/exmpp/include run_common_test.erl
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
		-s run_common_test ct

console:
	test -e escalus/Makefile || git submodule update --init --recursive
	cd escalus; make
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \

.PHONY: escalus/ebin
