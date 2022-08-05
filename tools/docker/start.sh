#!/usr/bin/env bash

#set -x
MIM_WORK_DIR="/usr/lib"
ls /member
cd /member
[ -f /member/hosts ] && cat /member/hosts >> /etc/hosts
cd -

NODE_TYPE=${NODE_TYPE:-"sname"}
[ "${NODE_TYPE}" = "sname" ] && NODE_HOST=$(hostname -s)
[ "${NODE_TYPE}" = "name" ]  && NODE_HOST=$(hostname -f)

# As HOSTNAME is not directly related to $(hostname -s) / $(hostname -f)
# we cannot rely on it
HOSTNAME_SHORT=$(hostname -s)

NODE_NAME=${NODE_NAME:-"mongooseim"}
NODE=${NODE_NAME}@${NODE_HOST}
CLUSTER_COOKIE=${CLUSTER_COOKIE:-"mongooseim"}
MASTER_ORDINAL=${MASTER_ORDINAL:-"1"}
ROOT_DIR=${MIM_WORK_DIR}/mongooseim
MNESIA_DIR=/var/lib/mongooseim/Mnesia.${NODE}
LOGS_DIR=/var/log/mongooseim
EPMD=`find ${ROOT_DIR} -name epmd`
ESCRIPT=`find ${ROOT_DIR} -name escript`
ETC_DIR=${ROOT_DIR}/etc

# if there are predefined config files available, use them
FILES=( "mongooseim.toml" "app.config" "vm.args" "vm.dist.args" )
for file in "${FILES[@]}"
do
    [ -f "/member/${file}" ] && ln -sf "/member/${file}" ${ETC_DIR}/${file}
done

# make sure proper node name is used
echo "vm.args:"
sed -i -E -e "s/^-s?name.*$/-${NODE_TYPE} ${NODE}/" ${ETC_DIR}/vm.args
cat ${ETC_DIR}/vm.args

echo "app.config"
sed -i -e "s,%{mnesia.*,{mnesia\, [{dir\, \"${MNESIA_DIR}\"}]}\,," ${ETC_DIR}/app.config
sed -i -e "s,{log_root.*,{log_root\, \"/var/log/mongooseim\"}\,," ${ETC_DIR}/app.config
cat ${ETC_DIR}/app.config

echo "vm.dist.args"
cat ${ETC_DIR}/vm.dist.args

#file "${MNESIA_DIR}/schema.DAT"

mkdir -p /var/lib/mongooseim
mkdir -p ${LOGS_DIR}

PATH="${MIM_WORK_DIR}/mongooseim/bin:${PATH}"

function run() {
    if [ "$#" -ne 1 ]; then
        mongooseim live --noshell -noinput +Bd
    else
        mongooseimctl $1
    fi
}

DEFAULT_CLUSTERING=0
if [ x"${CLUSTER_WITH}" = x"" ]; then
    # For short hostname - HOST_TAIL will be empty
    # For long hostname - HOST_TAIL will contain all of it but the leading segment
    HOST_TAIL=$(echo $NODE_HOST | sed -e 's/^[^.]*//')

    CLUSTER_WITH="${NODE_NAME}@${HOSTNAME_SHORT%-*}-${MASTER_ORDINAL}${HOST_TAIL}"
    DEFAULT_CLUSTERING=1
fi

if [ x"${JOIN_CLUSTER}" = x"" ] || [ "${JOIN_CLUSTER}" = "true" ] || [ "${JOIN_CLUSTER}" = "1" ]; then
    CLUSTERING_RESULT=0
    # don't cluster if default clustering is used and out suffix is -1
    if [ $DEFAULT_CLUSTERING -eq 1 ] && [ x"${HOSTNAME_SHORT##*-}" = x"${MASTER_ORDINAL}" ]; then
        echo "MongooseIM cluster primary node ${NODE}"
    elif [ ! -f "${MNESIA_DIR}/schema.DAT" ]; then
        echo "MongooseIM node ${NODE} joining ${CLUSTER_WITH}"
        mongooseimctl start
        mongooseimctl started
        mongooseimctl status
        mongooseimctl join_cluster -f ${CLUSTER_WITH}
        CLUSTERING_RESULT=$?
        mongooseimctl stop
    else
        echo "MongooseIM node ${NODE} already clustered"
    fi

    if [ ${CLUSTERING_RESULT} == 0 ]; then
        echo "Clustered ${NODE} with ${CLUSTER_WITH}"
        run
    else
        echo "Failed clustering ${NODE} with ${CLUSTER_WITH}"
        exit 2
    fi
else
    run
fi
