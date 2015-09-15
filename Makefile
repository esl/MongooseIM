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
	rm -rf apps/*/logs
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

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make qct SUITE=amp_resolver_SUITE
qct:
	mkdir -p /tmp/ct_log
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test\
        -I apps/*/include -logdir /tmp/ct_log -suite $(SUITE) -noshell

test: test_deps
	cd test/ejabberd_tests; make test

test_preset: test_deps
	cd test/ejabberd_tests; make test_preset


run: deps compile quickrun

quickrun: etc/ejabberd.cfg certs_priv
	erl -sname mongooseim@localhost -setcookie ejabberd -pa ebin deps/*/ebin apps/*/ebin -config rel/files/app.config -s mongooseim

etc/ejabberd.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/ejabberd.cfg

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

rel: certs rebar deps
	./rebar compile generate -f

devrel: certs $(DEVNODES)

$(DEVNODES): rebar deps compile deps_dev
	@echo "building $@"
	(cd rel && ../rebar generate -f target_dir=../dev/mongooseim_$@ overlay_vars=./reltool_vars/$@_vars.config)
	cp -R `dirname $(shell ./readlink.sh $(shell which erl))`/../lib/tools-* dev/mongooseim_$@/lib/

deps_dev:
	mkdir -p dev

devclean:
	rm -rf dev/*

cover_report: /tmp/mongoose_combined.coverdata
	erl -noshell -pa apps/*/ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

relclean:
	rm -rf rel/mongooseim

certs: fake_cert.pem fake_server.pem

certs_priv: certs
	@mkdir -p priv/ssl
	@cp fake_*.pem priv/ssl

fake_cert.pem:
	openssl req \
	-x509 -nodes -days 365 \
	-subj '/C=PL/ST=ML/L=Krakow/CN=mongoose-im' \
	-newkey rsa:2048 -keyout fake_key.pem -out fake_cert.pem

fake_server.pem:
	cat fake_cert.pem fake_key.pem > fake_server.pem

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


test_deps:
	cd test/ejabberd_tests; make get-deps

%:
	@:
