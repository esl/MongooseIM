# unit tests
make ct SUITE=mod_foreign_http_SUITE

# integration
make quicktest TESTSPEC=custom.spec

# build
make devrel DEVNODES=mim1

# run
_build/mim1/rel/mongooseim/bin/mongooseimctl live
