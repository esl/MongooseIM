.PHONY: all test

all: test

test_clean: escalus/ebin
	rm -rf tests/*.beam
	make test

test: escalus/ebin
	rm -rf ct_report
	mkdir  ct_report
	erlc -I escalus/deps/exmpp/include run_common_test.erl
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		-pa `pwd`/escalus/ebin \
		`pwd`/escalus/deps/exmpp/ebin -s run_common_test ct

escalus/ebin:
	(cd ../; git submodule init; git submodule update)
	(cd escalus; make)

console:
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		-pa `pwd`/escalus/ebin \
		-pa `pwd`/../tools \
		`pwd`/escalus/deps/exmpp/ebin

.PHONY: escalus/ebin
