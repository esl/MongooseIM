.PHONY: all test console compile
all: test

test_clean: escalus/Makefile
	rm -rf tests/*.beam
	make test

cover_test_clean: escalus/Makefile
	rm -rf tests/*.beam
	make cover_test

test: escalus/Makefile prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
			`pwd`/escalus/deps/base16/ebin \
		-s run_common_test ct

cover_test: escalus/Makefile prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
			`pwd`/escalus/deps/base16/ebin \
		-s run_common_test ct_cover

cover_summary: escalus/Makefile prepare
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
			`pwd`/escalus/ebin \
			`pwd`/escalus/deps/exml/ebin \
			`pwd`/escalus/deps/lxmppc/ebin \
			`pwd`/escalus/deps/base16/ebin \
		-s run_common_test cover_summary

prepare:
	cd escalus; make
	erlc -Iescalus/deps/exml/include \
		 -Iescalus/deps/lxmppc/include \
		 run_common_test.erl
	mkdir -p ct_report

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
