.PHONY: rel deps test show_test_results

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
DEVNODES = node1 node2

all: deps compile

compile: rebar
	./rebar $(OTPS) compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

quick_compile: rebar
	./rebar $(OPTS) compile skip_deps=true

reload: quick_compile
	@E=`ls ./rel/mongooseim/lib/ | grep ejabberd-2 | sort -r | head -n 1` ;\
	rsync -uW ./apps/ejabberd/ebin/*beam ./rel/mongooseim/lib/$$E/ebin/ ;\

reload_dev: quick_compile
	@E=`ls ./dev/mongooseim_node1/lib/ | grep ejabberd-2 | sort -r | head -n 1` ;\
	rsync -uW ./apps/ejabberd/ebin/*beam ./dev/mongooseim_node1/lib/$$E/ebin/ ;\

ct: deps quick_compile
	@if [ "$(SUITE)" ]; then ./rebar -q ct suite=$(SUITE) skip_deps=true;\
	else ./rebar -q ct skip_deps=true; fi

test: test_deps
	cd test/ejabberd_tests; make test

test_preset: test_deps
	cd test/ejabberd_tests; make test_preset


run: deps compile quickrun

quickrun:
	erl -sname ejabberd -setcookie ejabberd -pa deps/*/ebin apps/*/ebin -config rel/files/app.run.config -s ejabberd -s sync

cover_test: test_deps
	cd test/ejabberd_tests; make cover_test

cover_test_preset: test_deps
	cd test/ejabberd_tests; make cover_test_preset

quicktest: test_deps
	cd test/ejabberd_tests; make quicktest

show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

eunit: rebar deps
	./rebar compile
	./rebar skip_deps=true eunit

configure:
	./tools/configure $(filter-out $@,$(MAKECMDGOALS))

rel: rebar deps
	./rebar compile generate -f

devrel: $(DEVNODES)

$(DEVNODES): rebar deps compile deps_dev
	@echo "building $@"
	(cd rel && ../rebar generate -f target_dir=../dev/mongooseim_$@ overlay_vars=./reltool_vars/$@_vars.config)
	cp -R `dirname $(shell ./readlink.sh $(shell which erl))`/../lib/tools-* dev/mongooseim_$@/lib/

deps_dev:
	mkdir -p dev
	cp rel/files/test_cert.pem /tmp/server.pem
	cp rel/files/sample_external_auth.py /tmp

devclean:
	rm -rf dev/*

cover_report: /tmp/mongoose_combined.coverdata
	erl -noshell -pa apps/*/ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

relclean:
	rm -rf rel/mongooseim

COMBO_PLT = .mongooseim_combo_dialyzer.plt
DEPS_LIBS     = $(wildcard deps/*/ebin/*.beam)
MONGOOSE_LIBS = $(wildcard apps/ejabberd/ebin/*.beam)

OTP_APPS      = compiler crypto erts kernel stdlib mnesia ssl ssh
DIALYZER_APPS = ejabberd
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt:
	dialyzer --check_plt --plt $(COMBO_PLT) $(MONGOOSE_LIBS)

build_plt:
	dialyzer --build_plt --apps $(OTP_APPS) \
		--output_plt $(COMBO_PLT) $(DEPS_LIBS) $(MONGOOSE_LIBS)

dialyzer: compile
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log

cleanplt:
	rm $(COMBO_PLT)


test_deps: rebar
	./rebar -C rebar.tests.config get-deps

%:
	@:
