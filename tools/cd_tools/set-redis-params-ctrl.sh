#!/bin/bash

#Purpose:
#Set all REDIS specific connection string fields passed as parameters to this script.
#Used by:
#Go-CD, Linux.
#Note:
#Script performs in-place sed injections to set-odbc-params.sh script which is run to
#modify test spec file. Because target files are small this should do the job. Otherwise
#consider using Awk or Perl to do the job.

if [ $# -ne 2  ]
then
        echo "USAGE: set-redis-params-ctrl dbhost, port"
else
    cp set-redis-params.sh.template set-redis-params.sh
    SCRIPT_TO_PREPARE='set-redis-params.sh'
    sed "s/HOST/$1/g" -i ${SCRIPT_TO_PREPARE}
    sed "s/PORT/$2/g" -i ${SCRIPT_TO_PREPARE}
    source set-redis-params.sh runinplace
fi
