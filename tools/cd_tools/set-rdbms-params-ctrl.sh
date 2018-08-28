#!/bin/bash

#Purpose:
#Set all RDBMS specific connection string fields passed as parameters to this script.
#Used by:
#Go-CD, Linux.
#Note:
#Script performs in-place sed injections to set-rdbms-params.sh script which is run to
#modify test spec file. Because target files are small this should do the job. Otherwise
#consider using Awk or Perl to do the job.

if [ $# -le 4 ]
then
        echo "USAGE: set-rdbms-params-ctrl dbtype, dbhost, username, databasename, password"
else
    cp set-rdbms-params.sh.template set-rdbms-params.sh
    SCRIPT_TO_PREPARE='set-rdbms-params.sh'
    sed "s/DBTYPE/$1/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/HOST/$2/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/USER/$3/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/DATABASENAME/$4/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/DBPASSWORD/$5/g" -i ${SCRIPT_TO_PREPARE}
    source set-rdbms-params.sh runinplace
fi
