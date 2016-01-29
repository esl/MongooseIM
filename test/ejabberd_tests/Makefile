.PHONY: all test console compile
all: test

TESTSPEC ?= default.spec
PRESET   ?= all
PREPARE  ?= prepare

test_clean: get-deps
	rm -rf tests/*.beam
	make test

cover_test_clean: get-deps
	rm -rf tests/*.beam
	make cover_test

quicktest: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=quick spec=$(TESTSPEC) \
		> $@.log 2>&1 || (cat $@.log; exit 1)

cover_quicktest: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=quick spec=$(TESTSPEC) cover=true \
		> $@.log 2>&1 || (cat $@.log; exit 1)

test_preset: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=full spec=$(TESTSPEC) preset=$(PRESET) \
		> $@.log 2>&1 || (cat $@.log; exit 1)

cover_test_preset: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=full spec=$(TESTSPEC) preset=$(PRESET) cover=true \
		> $@.log 2>&1 || (cat $@.log; exit 1)

test: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=full spec=$(TESTSPEC) \
		> $@.log 2>&1 || (cat $@.log; exit 1)

cover_test: $(PREPARE)
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \
		$(ADD_OPTS) \
		-s run_common_test main test=full spec=$(TESTSPEC) cover=true \
		> $@.log 2>&1 || (cat $@.log; exit 1)

prepare: compile
	erlc -Ideps/exml/include \
		 run_common_test.erl
	mkdir -p ct_report

console: compile
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/tests \
		    `pwd`/ebin \
			`pwd`/deps/*/ebin \

compile: get-deps
	./rebar compile > $@.log 2>&1 || (cat $@.log; exit 1)

get-deps: rebar
	./rebar get-deps > $@.log 2>&1 || (cat $@.log; exit 1)

clean: rebar
	-@rm -rf tests/*.beam > /dev/null 2>&1
	-@./rebar clean > /dev/null 2>&1

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
