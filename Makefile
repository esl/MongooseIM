.PHONY: rel

RUN=./tools/silent_exec.sh "$@.log"
XEP_TOOL = tools/xep_tool
EBIN = ebin
DEVNODES = mim1 mim2 mim3 fed1 reg1
REBAR=./rebar3

# Top-level targets aka user interface

all: rel

clean:
	-$(REBAR) clean
	-rm -rf _build
	-rm -rf asngen
	-rm configure.out
	-rm rel/configure.vars.config

# REBAR_CT_EXTRA_ARGS comes from a test runner
ct:
	@(if [ "$(SUITE)" ]; \
		then $(RUN) $(REBAR) ct --dir test --suite $(SUITE) ; \
		else $(RUN) $(REBAR) ct $(REBAR_CT_EXTRA_ARGS); fi)

eunit:
	@$(RUN) $(REBAR) eunit $(REBAR_EUNIT_EXTRA_ARGS)

rel: certs configure.out rel/configure.vars.config
	. ./configure.out && $(REBAR) as prod release

shell: certs etc/mongooseim.cfg
	$(REBAR) shell

# Top-level targets' dependency chain

rock:
	@if [ "$(FILE)" ]; then elvis rock $(FILE);\
	elif [ "$(BRANCH)" ]; then tools/rock_changed.sh $(BRANCH); \
	else tools/rock_changed.sh; fi

## Don't allow these files to go out of sync!
configure.out rel/configure.vars.config:
	./tools/configure with-all without-jingle-sip

etc/mongooseim.cfg:
	@mkdir -p $(@D)
	tools/generate_cfg.es etc/mongooseim.cfg rel/files/mongooseim.cfg

devrel: $(DEVNODES)

print_devnodes:
	@echo $(DEVNODES)

$(DEVNODES): certs configure.out rel/vars-toml.config
	@echo "building $@"
	(. ./configure.out && \
	DEVNODE=true $(RUN) $(REBAR) as $@ release)

maybe_clean_certs:
	if [ "$$SKIP_CERT_BUILD" != 1 ]; then \
		if ! openssl x509 -checkend 3600 -noout -in tools/ssl/ca/cacert.pem ; then \
			cd tools/ssl && make clean_certs; \
		fi \
	fi

certs: maybe_clean_certs
	if [ "$$SKIP_CERT_BUILD" = 1 ]; then \
		echo "Skip cert build"; \
	else \
		cd tools/ssl && make; \
	fi

xeplist:
	$(XEP_TOOL)/xep_tool.escript doap doc/mongooseim.doap
	$(XEP_TOOL)/xep_tool.escript json doc/supported-xeps.json
	$(XEP_TOOL)/xep_tool.escript markdown doc/user-guide/Supported-XEPs.md

install: configure.out rel
	@. ./configure.out && tools/install

elvis:
	$(REBAR) as lint lint
