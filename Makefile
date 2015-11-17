fake change

.PHONY: rel deps test show_test_results

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
DEVNODES = node1 node2
DEVNODESCD = node1cd node2cd
REL_DEST = ./uat_package
COVER_SOURCES = ./cover_sources

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

quickrun: etc/ejabberd.cfg etc/app.config certs_priv
	erl -sname mongooseim@localhost -setcookie ejabberd -pa ebin deps/*/ebin apps/*/ebin -config etc/app.config -s mongooseim

etc/ejabberd.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/ejabberd.cfg rel/files/ejabberd.cfg

etc/app.config:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/app.config rel/files/app.config

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
# We skip some deps, because they're Dialyzer-broken
BANNED_DEPS = meck edown
BANNED_PATHS = $(addsuffix /ebin, $(addprefix deps/, $(BANNED_DEPS)))
DEPS_LIBS = $(filter-out $(BANNED_PATHS), $(wildcard deps/*/ebin))
MONGOOSE_LIBS = $(wildcard apps/ejabberd/ebin/*.beam)

OTP_APPS = compiler crypto erts kernel stdlib mnesia ssl ssh xmerl public_key tools sasl hipe edoc syntax_tools runtime_tools inets webtool asn1
DIALYZER_APPS = ejabberd mysql pgsql
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt:
	dialyzer --check_plt --plt $(COMBO_PLT)

build_plt:
	dialyzer --build_plt --apps $(OTP_APPS) --output_plt $(COMBO_PLT) $(DEPS_LIBS)

dialyzer: check_plt dialyzer_quick

dialyzer_quick:
	dialyzer -n -Wno_return -Wno_unused -Wno_undefined_callbacks --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS)
#	    fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log

cleanplt:
	rm $(COMBO_PLT)


test_deps:
	cd test/ejabberd_tests; make get-deps

# CONTINUOUS DELIVERY TARGETS invoked by building/testing agents

# after successful build and common tests passed - expose all binaries for uat testing.
# additional files for release generation should be included as well.
cd_copyrel:
	mkdir $(REL_DEST)
	rsync -uWr --exclude="*.erl" --exclude="*.spec" --exclude=".git" --exclude="logs/" --exclude="test/" ./apps $(REL_DEST)
	rsync -uWr --exclude="*.erl" --exclude="*.spec" --exclude=".git" --exclude="test/" ./deps $(REL_DEST)
	rsync -uWr --exclude="log/" ./rel $(REL_DEST)
	rsync -uWr rebar $(REL_DEST)
	rsync -uWr rebar.config $(REL_DEST)
	rsync -uWr rebar.config.script $(REL_DEST)
	rsync -uWr Makefile $(REL_DEST)
	rsync -uWr readlink.sh $(REL_DEST)
	rsync -uWr ./ebin $(REL_DEST)
	rsync -uWr --exclude="*.erl" ./src $(REL_DEST)
	rsync -uWr ./tools/configure $(REL_DEST)
	rsync -uWr ./tools/provision_pgsql.sh $(REL_DEST)
	tar -cf uat_package.tar $(REL_DEST)/*

# copy selected sources so that test "presets" still can be used as in Travis
cd_copy_for_cover:
	mkdir $(COVER_SOURCES)
	rsync -uWr ./rel $(COVER_SOURCES)
	rsync -uWr --exclude="*.spec" --exclude=".git" --exclude="logs/" --exclude="test/" ./apps $(COVER_SOURCES)
	tar -cf cover_sources.tar $(COVER_SOURCES)/*

cd_copyrel_unpack:
	mv ./* ../


# ---------------- UAT test release , 2 nodes. It ONLY generates testable releases to "dev" folder.
cd_two_nodes_release: $(DEVNODESCD)

# run very basic test to check if release has been generated properly
cd_release_smoketest:
	@echo "this configuration ($@) seems to work!"

$(DEVNODESCD): rebar
	@echo "building $@"
	(cd rel && ../rebar generate -f target_dir=../dev/mongooseim_$@ overlay_vars=./reltool_vars/$@_vars.config)
	cp -R /usr/OTP_174/lib/tools-* dev/mongooseim_$@/lib/
#cp -R $(erl_tools) dev/mongooseim_$@/lib/    - ucina sciezke do toolsow w go-cd serwerze. zaraz cos mnie trafi.

# UAT test release, minimalistic 1-node deployment to dev. todo: change cp to relative paths
cd_release_base: rebar
	(cd rel && ../rebar generate -f target_dir=../dev/mynode overlay_vars=./reltool_vars/mynode_vars.config)
	cp -R /usr/OTP_174/lib/tools-* dev/mynode/lib/

# run very basic test to check if release has been generated properly
cd_release_base_smoketest:
	@echo "this configuration ($@) seems to work!"

# called by external integration tool. prerequisites: targets deps,compile
# have been called already.
cd_ct:
	mkdir -v apps/ejabberd/ctlogs
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test\
        -I apps/*/include -logdir apps/ejabberd/ctlogs  -noshell
# -ct_hooks cth_surefire -logdir apps/ejabberd/ct_surefire_logs

# --runtime control---- start test deployments for varios UAT/functional test targets.
# After starting - invote ALL/some functional user acceptance tests.
# -------- against configurations.

cd_uat_test_two_nodes_start:
	cd mongooseim_node1cd/bin && ./mongooseimctl start
	sleep 3
	cd mongooseim_node2cd/bin && ./mongooseimctl start

cd_uat_test_two_nodes_stop:
	cd mongooseim_node1cd/bin && ./mongooseimctl stop
	sleep 3
	cd mongooseim_node2cd/bin && ./mongooseimctl stop

cd_uat_test_full:
	cd ejabberd_tests_for_execution/test/ejabberd_tests && make test

%:
	@:
