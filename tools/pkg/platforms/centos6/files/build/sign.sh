#!/bin/bash
#gpg --export -a 'Erlang Solutions Ltd.' > RPM-GPG-KEY
#sudo rpm --import RPM-GPG-KEY
#
set -e
source ../../common.sh
cd mongooseim
export ERL_TOP=`pwd`
yes "" | rpm --resign rpmbuild/RPMS/*/*.rpm
_prog 100 "finished"
