.PHONY: rel
LOG=$(subst TARGET,$@,TARGET.log 2>&1 || (cat TARGET.log; exit 1))
SILENCE_COVER =  | grep -v "logs.*\\.coverdata"
SILENCE_COVER += | grep -v "Analysis includes data from imported files"
SILENCE_COVER += | grep -v "WARNING: Deleting data for module"
LOG_SILENCE_COVER=$(subst TARGET,$@,TARGET.log 2>&1 || (cat TARGET.log $(SILENCE_COVER); exit 1))
EJABBERD_DIR = apps/ejabberd
XEP_TOOL = tools/xep_tool
EJD_EBIN = $(EJABBERD_DIR)/ebin
DEVNODES = mim1 mim2 mim3 fed1

# Top-level targets aka user interface

all: rel

dev: $(DEVNODES)

clean:
	-rm -rf _build
	-rm rel/configure.vars.config
	-rm rel/vars.config

ct:
	@(if [ "$(SUITE)" ]; then ./rebar3 ct --dir apps/ejabberd/test --suite $(SUITE) ;\
		else ./rebar3 ct ; fi) > $(LOG_SILENCE_COVER)

rel: certs configure.out rel/vars.config
	. ./configure.out && ./rebar3 as prod release

shell: certs etc/ejabberd.cfg
	./rebar3 shell

# Top-level targets' dependency chain

rock:
	@if [ "$(FILE)" ]; then elvis rock $(FILE);\
	elif [ "$(BRANCH)" ]; then tools/rock_changed.sh $(BRANCH); \
	else tools/rock_changed.sh; fi

rel/vars.config: rel/vars.config.in rel/configure.vars.config
	cat $^ > $@

## Don't allow these files to go out of sync!
configure.out rel/configure.vars.config:
	./tools/configure with-all

etc/ejabberd.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/ejabberd.cfg rel/files/ejabberd.cfg

devrel: $(DEVNODES)

$(DEVNODES): certs configure.out rel/vars.config
	@echo "building $@"
	(. ./configure.out && \
	DEVNODE=true ./rebar3 as $@ release) > $(LOG_SILENCE_COVER)

certs: tools/ssl/ca/cacert.pem tools/ssl/fake_cert.pem \
       tools/ssl/fake_key.pem tools/ssl/fake_server.pem \
	   tools/ssl/fake_dh_server.pem

tools/ssl/ca/cacert.pem tools/ssl/fake_cert.pem \
tools/ssl/fake_key.pem tools/ssl/fake_server.pem \
tools/ssl/fake_dh_server.pem:
	cd tools/ssl && make

xeplist: escript
	escript $(XEP_TOOL)/xep_tool.escript markdown $(EJD_EBIN)

install: configure.out rel
	@. ./configure.out && tools/install

cover_report: /tmp/mongoose_combined.coverdata
	erl -noshell -pa _build/default/lib/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()' > $(LOG)
