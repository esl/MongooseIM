.PHONY: rel deps test show_test_results generate_snmp_header 

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
EJD_PRIV_MIB = $(EJD_PRIV)/mibs
EJD_MIB = $(EJABBERD_DIR)/mibs
DEVNODES = node1 node2

all: deps compile

compile: rebar generate_snmp_header
	./rebar compile

deps: rebar generate_snmp_header
	./rebar get-deps

clean: rebar
	./rebar clean

test: test_deps
	cd test/ejabberd_tests; make test

test_config: test_deps
	cd test/ejabberd_tests; make test_config

cover_test: test_deps
	cd test/ejabberd_tests; make cover_test

quicktest: test_deps
	cd test/ejabberd_tests; make quicktest

show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

eunit: rebar
	./rebar skip_deps=true eunit

rel: rebar deps
	./rebar compile generate -f

devrel: $(DEVNODES)

$(DEVNODES): rebar deps compile deps_dev
	@echo "building $@"
	(cd rel && ../rebar generate -f target_dir=../dev/mongooseim_$@ overlay_vars=./reltool_vars/$@_vars.config)
	cp apps/ejabberd/src/*.erl `ls -dt dev/mongooseim_$@/lib/ejabberd-2.1.8*/ebin/ | head -1`
ifeq ($(shell uname), Linux)
	cp -R `dirname $(shell readlink -f $(shell which erl))`/../lib/tools-* dev/mongooseim_$@/lib/
else
	cp -R `which erl`/../../lib/tools-* dev/mongooseim_$@/lib/
endif

deps_dev:
	mkdir -p dev
	cp rel/files/test_cert.pem /tmp/server.pem
	cp rel/files/sample_external_auth.py /tmp

devclean:
	rm -rf dev/*

generate_snmp_header: apps/ejabberd/include/EJABBERD-MIB.hrl

$(EJD_INCLUDE)/EJABBERD-MIB.hrl: $(EJD_PRIV_MIB)/EJABBERD-MIB.bin
	erlc -o $(EJD_INCLUDE) $<

$(EJD_PRIV_MIB)/EJABBERD-MIB.bin: $(EJD_MIB)/EJABBERD-MIB.mib $(EJD_MIB)/EJABBERD-MIB.funcs
	erlc -o $(EJD_PRIV_MIB) $<

relclean:
	rm -rf rel/mongooseim

COMBO_PLT = $(HOME)/.esl_ejabberd_combo_dialyzer_plt
PLT_LIBS  = $(wildcard rel/mongooseim/lib/*/ebin)

DIALYZER_APPS = ejabberd
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

build_plt: rel
	dialyzer --build_plt --output_plt $(COMBO_PLT) $(PLT_LIBS)

dialyzer: compile
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	rm $(COMBO_PLT)

test_deps: rebar
	./rebar -C rebar.tests.config get-deps

# This might download a version which can't build the project properly!
# Compatible rebar version should be checked into the repository.
rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

