.PHONY: rel

RUN=./tools/silent_exec.sh "$@.log"
XEP_TOOL = tools/xep_tool
EBIN = ebin
DEVNODES = mim1 mim2 mim3 fed1 reg1
REBAR=./rebar3

# Top-level targets aka user interface

all: rel

clean:
	-rm -rf asngen
	-rm -rf _build
	-rm rel/configure.vars.config
	-rm rel/vars.config

# REBAR_CT_EXTRA_ARGS comes from a test runner
ct:
	@(if [ "$(SUITE)" ]; \
		then $(RUN) $(REBAR) ct --dir test --suite $(SUITE) ; \
		else $(RUN) $(REBAR) ct $(REBAR_CT_EXTRA_ARGS); fi)

rel: certs configure.out rel/vars.config
	. ./configure.out && $(REBAR) as prod release

shell: certs etc/mongooseim.cfg
	$(REBAR) shell

# Top-level targets' dependency chain

rock:
	@if [ "$(FILE)" ]; then elvis rock $(FILE);\
	elif [ "$(BRANCH)" ]; then tools/rock_changed.sh $(BRANCH); \
	else tools/rock_changed.sh; fi

rel/vars.config: rel/vars.config.in rel/configure.vars.config
	cat $^ > $@

## Don't allow these files to go out of sync!
configure.out rel/configure.vars.config:
	./tools/configure with-all without-jingle-sip

etc/mongooseim.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/mongooseim.cfg rel/files/mongooseim.cfg

devrel: $(DEVNODES)

print_devnodes:
	@echo $(DEVNODES)

$(DEVNODES): certs configure.out rel/vars.config
	@echo "building $@"
	(. ./configure.out && \
	DEVNODE=true $(RUN) $(REBAR) as $@ release)

certs:
	cd tools/ssl && $(MAKE)

xeplist:
	escript $(XEP_TOOL)/xep_tool.escript markdown $(EBIN)

install: configure.out rel
	@. ./configure.out && tools/install

elvis:
	$(REBAR) as lint lint
