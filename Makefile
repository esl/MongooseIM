.PHONY: all test console
all: test

test_clean: escalus/Makefile
	rm -rf tests/*.beam
	make test

test: escalus/Makefile
	cd escalus; make
	erlc -Iescalus/deps/exml/include \
		 -Iescalus/deps/lxmppc/include \
		 run_common_test.erl
	mkdir -p ct_report
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
			`pwd`/escalus/deps/base16/ebin \
		-s run_common_test ct

console: escalus/Makefile
	cd escalus; make
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
			`pwd`/escalus/deps/base16/ebin

escalus/Makefile:
	git submodule update --init --recursive
