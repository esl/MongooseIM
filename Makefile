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
	@: TODO: find is temporary - relx fails when copying over an existing file
	@-find _build -name sample_external_auth.py -exec rm '{}' \;
	@(if [ "$(SUITE)" ]; then ./rebar3 ct --suite $(SUITE) ;\
		else ./rebar3 ct ; fi) > $(LOG_SILENCE_COVER)

rel: certs configure.out rel/vars.config
	. ./configure.out && ./rebar3 as prod release

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

devrel: $(DEVNODES)

$(DEVNODES): certs configure.out rel/vars.config
	@echo "building $@"
	(. ./configure.out && \
	 ./rebar3 as $@ release) > $(LOG_SILENCE_COVER)

certs: fake_cert.pem fake_server.pem fake_dh_server.pem

fake_cert.pem:
	openssl req \
	-x509 -nodes -days 365 \
	-subj '/C=PL/ST=ML/L=Krakow/CN=mongoose-im' \
	-newkey rsa:2048 -keyout fake_key.pem -out fake_cert.pem

fake_server.pem:
	cat fake_cert.pem fake_key.pem > fake_server.pem

fake_dh_server.pem:
	openssl dhparam -outform PEM -out fake_dh_server.pem 1024

include dialyzer.mk

xeplist: escript
	escript $(XEP_TOOL)/xep_tool.escript markdown $(EJD_EBIN)

install: configure.out rel
	@. ./configure.out && tools/install

include tools/cd_tools/cd-targets
