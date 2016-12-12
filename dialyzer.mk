.PHONY: dialyzer
# We skip some deps, because they're Dialyzer-broken
BANNED_DEPS = meck edown
BANNED_PATHS = $(addsuffix /ebin, $(addprefix deps/, $(BANNED_DEPS)))
DEPS_LIBS = $(filter-out $(BANNED_PATHS), $(wildcard deps/*/ebin))

OTP_APPS = compiler crypto erts kernel stdlib mnesia ssl ssh xmerl public_key tools sasl hipe edoc syntax_tools runtime_tools inets asn1

clean_dialyzer:
	rm -rf dialyzer

apps := $(wildcard apps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps $(OTP_APPS); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(DEPS_LIBS); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/apps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/apps.plt \
	-o dialyzer/apps.log $(apps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

apps_plt: dialyzer/apps.plt
	@dialyzer --plt dialyzer/apps.plt --check_plt -o dialyzer/apps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt apps_plt
	@dialyzer -n --plts dialyzer/*.plt --verbose \
	--get_warnings $(apps);
#	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi


