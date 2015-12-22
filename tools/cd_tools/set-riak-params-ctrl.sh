#!/bin/bash

#Purpose:
#Set all RIAK specific connection string fields passed as parameters to this script.
#Used by:
#Go-CD, Linux.
#Note:
#Script performs in-place sed injections to set-odbc-params.sh script which is run to
#modify test spec file. Because target files are small this should do the job. Otherwise
#consider using Awk or Perl to do the job.

if [ $# -ne 1  ]
then
        echo "USAGE: set-riak-params-ctrl dbhost"
else
    cp set-riak-params.sh.template set-riak-params.sh
    SCRIPT_TO_PREPARE='set-riak-params.sh'
    sed "s/HOST/$1/g" -i ${SCRIPT_TO_PREPARE}
    source set-riak-params.sh runinplace
fi
