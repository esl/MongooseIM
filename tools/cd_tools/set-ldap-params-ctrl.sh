#!/bin/bash

#Purpose:
#Set all LDAP specific connection string fields passed as parameters to this script.
#Used by:
#Go-CD, Linux.
#Note:
#Script performs in-place sed injections to set-ldap-params.sh script which is run to
#modify test spec file. Because target files are small this should do the job. Otherwise
#consider using Awk or Perl to do the job.

if [ $# -ne 2 ]
then
        echo "USAGE: set-ldap-params-ctrl dbhost, password"
else
    cp set-ldap-params.sh.template set-ldap-params.sh
    SCRIPT_TO_PREPARE='set-ldap-params.sh'
    sed "s/HOST/$1/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/DBPASSWORD/$2/g" -i ${SCRIPT_TO_PREPARE}
    source set-ldap-params.sh runinplace
fi
