LOG=$(subst TARGET,$@,TARGET.log 2>&1 || (cat TARGET.log; exit 1))
EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
XEP_TOOL = tools/xep_tool
EJD_EBIN = $(EJABBERD_DIR)/ebin
DEVNODES = node1 node2 node3 fed1

.PHONY: all
all: deps compile

.PHONY: compile
compile: rebar
	./rebar $(OPTS) compile > $(LOG)

deps: rebar
	./rebar get-deps > $(LOG)

.PHONY: clean
clean: rebar configure.out
	rm -rf apps/*/logs
	. ./configure.out && ./rebar clean

.PHONY: quick_compile
quick_compile: rebar
	./rebar $(OPTS) compile skip_deps=true > $(LOG)

.PHONY: reload
reload: quick_compile
	@E=`ls ./rel/mongooseim/lib/ | grep ejabberd-2 | sort -r | head -n 1` ;\
	rsync -uW ./apps/ejabberd/ebin/*beam ./rel/mongooseim/lib/$$E/ebin/ ;\

.PHONY: reload_dev
reload_dev: quick_compile
	@for NODE in $(DEVNODES); do \
		E=`ls ./dev/mongooseim_$$NODE/lib/ | grep ejabberd-2 | sort -r | head -n 1` ;\
		rsync -uW ./apps/ejabberd/ebin/*beam ./dev/mongooseim_$$NODE/lib/$$E/ebin/ ;\
	done

.PHONY: ct
ct: deps quick_compile
	@(if [ "$(SUITE)" ]; then ./rebar ct suite=$(SUITE) skip_deps=true;\
		else ./rebar ct skip_deps=true; fi) > $(LOG)

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make qct SUITE=amp_resolver_SUITE
.PHONY: qct
qct:
	mkdir -p /tmp/ct_log
	@if [ "$(SUITE)" ]; then ct_run -pa apps/*/ebin -pa deps/*/ebin -pa ebin -dir apps/*/test\
        -I apps/*/include -I apps/*/src -logdir /tmp/ct_log -suite $(SUITE)_SUITE -noshell;\
	else ct_run -pa apps/*/ebin -pa deps/*/ebin -pa ebin -dir apps/*/test\
        -I apps/*/include -I apps/*/src -logdir /tmp/ct_log -noshell; fi

.PHONY: test
test: test_deps
	cd test/ejabberd_tests; make test

.PHONY: test_preset
test_preset: test_deps
	cd test/ejabberd_tests; make test_preset

.PHONY: run
run: deps compile quickrun

.PHONY: quickrun
quickrun: etc/ejabberd.cfg etc/app.config certs_priv
	erl -sname mongooseim@localhost -setcookie ejabberd \
		-pa ebin deps/*/ebin apps/*/ebin -config etc/app.config \
		-s mongooseim

etc/ejabberd.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/ejabberd.cfg rel/files/ejabberd.cfg

etc/app.config:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/app.config rel/files/app.config

.PHONY: cover_test
cover_test: test_deps
	cd test/ejabberd_tests; make cover_test

.PHONY: cover_test_preset
cover_test_preset: test_deps
	cd test/ejabberd_tests; make cover_test_preset

.PHONY: quicktest
quicktest: test_deps
	cd test/ejabberd_tests; make quicktest

.PHONY: show_test_results
show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

.PHONY: eunit
eunit: rebar deps
	./rebar compile
	./rebar skip_deps=true eunit

.PHONY: rel
rel: certs rebar deps configure.out rel/vars.config
	. ./configure.out && ./rebar compile generate -f

rel/vars.config: rel/vars.config.in rel/configure.vars.config
	cat $^ > $@

## Don't allow these files to go out of sync!
configure.out rel/configure.vars.config:
	./tools/configure with-all

.PHONY: devrel
devrel: certs $(DEVNODES)

.PHONY: $(DEVNODES)
$(DEVNODES): rebar deps compile deps_dev configure.out rel/vars.config
	@echo "building $@"
	(. ./configure.out && \
	 cd rel && \
	 ../rebar generate -f target_dir=../dev/mongooseim_$@ overlay_vars=./reltool_vars/$@_vars.config) \
		> $(LOG)
	cp -R `dirname $(shell ./readlink.sh $(shell which erl))`/../lib/tools-* dev/mongooseim_$@/lib/

.PHONY: deps_dev
deps_dev: dev

dev:
	mkdir -p dev

.PHONY: devclean
devclean:
	-@rm -rf dev/* > /dev/null 2>&1

.PHONY: cover_report
cover_report: /tmp/mongoose_combined.coverdata
	erl -noshell -pa apps/*/ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()' > $(LOG)

.PHONY: relclean
relclean:
	rm -rf rel/mongooseim

.PHONY: certs
certs: fake_cert.pem fake_server.pem

.PHONY: certs_priv
certs_priv: certs priv/ssl
	@cp fake_*.pem priv/ssl

priv/ssl:
	@mkdir -p priv/ssl

fake_cert.pem:
	openssl req \
	-x509 -nodes -days 365 \
	-subj '/C=PL/ST=ML/L=Krakow/CN=mongoose-im' \
	-newkey rsa:2048 -keyout fake_key.pem -out fake_cert.pem

fake_server.pem:
	cat fake_cert.pem fake_key.pem > fake_server.pem

include dialyzer.mk

.PHONY: xeplist
xeplist: escript
	escript $(XEP_TOOL)/xep_tool.escript markdown $(EJD_EBIN)

.PHONY: test_deps
test_deps:
	cd test/ejabberd_tests; make get-deps

.PHONY: install
install: configure.out rel
	@. ./configure.out && tools/install

include tools/cd_tools/cd-targets
