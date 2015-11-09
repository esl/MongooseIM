#!/bin/bash

DOCKERFILE_HOME="~/docker-pgsql"
# DOCKERFILE="docker_pgsql"

INSTANCE_NAME="mongooseim-postgre"
IMAGE_NAME="mongooseim-postgresql-backend"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="docker_pgsql"

# First 3 parameters: db role to be created, db role password, db to be created
if [ $# -ge 3 ]
then
    ROLE_NAME=$1
    ROLE_DB_PASSWORD=$2
    DATABASE_NAME=$3
    echo "db role name: "  "$1"
    echo "db pass: " "$2"
    echo "db name: " "$3"
else
    ROLE_NAME='usermongooseim'
    ROLE_DB_PASSWORD='password'
    DATABASE_NAME='mongooseim'
fi

if [ $# -ge 4 ]
then
    echo "db prov sql: " "$4"
    PROVISIONING_SQL_FULLFILENAME=$4
    PROVISIONING_SQL_FILENAME="${PROVISIONING_SQL_FULLFILENAME##*/}"
    PROVISIONING_SQL_PATH=`dirname "${PROVISIONING_SQL_FULLFILENAME}"`
else
    PROVISIONING_SQL_FILENAME="pg.sql"
    PROVISIONING_SQL_PATH="/var/lib/go-agent/pipelines/Baseline-build/apps/ejabberd/priv"
fi

PROVISIONING_SQL_FULLFILENAME=${PROVISIONING_SQL_PATH}/${PROVISIONING_SQL_FILENAME}

echo "Provisioning SQL fullname is:" ${PROVISIONING_SQL_FULLFILENAME}

echo "stopping docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker stop '${INSTANCE_NAME}''
echo "removing docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker rm '${INSTANCE_NAME}''
echo "creating new container from dockerfile...";
# ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker build -t '${IMAGE_NAME}' -f '${DOCKERFILE_HOME}'/'${DOCKERFILE}' .'
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker build -t '${IMAGE_NAME}' '${DOCKERFILE_HOME}'/.'
echo "starting the container with postgresql..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker run --name '${INSTANCE_NAME}' -p 5432:5432 -d '${IMAGE_NAME}''


# PSQL SPECIFIC PART - provision database with role, schema and tables

echo "configuring postgresql..."
echo "CREATE ROLE ${ROLE_NAME} PASSWORD '${ROLE_DB_PASSWORD}' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN" > ~/createrole_sql;
rsync -avz -e ssh ~/createrole_sql ${SSH_DOCKERMACHINE_ALIAS}:/home/go;
echo "adding role - executing sql remotely";
while :
do
    ssh ${SSH_DOCKERMACHINE_ALIAS} psql '-h localhost -U postgres < ~/createrole_sql'
    if [ $? -eq 0 ]; then echo "provisioning database success"; break; fi
done

echo "checking if role has been added"
ssh ${SSH_DOCKERMACHINE_ALIAS} psql '-h localhost -U postgres -c "\du"' | grep -q ${ROLE_NAME}
if [ $? -eq 1 ]
then
    echo "error creating role"
else
    echo "role added successfully"
fi

echo "adding database"
ssh ${SSH_DOCKERMACHINE_ALIAS} 'psql -h localhost -U postgres -c "CREATE DATABASE '${DATABASE_NAME}'"'
# ssh ${SSH_DOCKERMACHINE_ALIAS} 'psql -e -h localhost -U ${ROLE_NAME} -c "CREATE DATABASE '${DATABASE_NAME}'"'
ssh ${SSH_DOCKERMACHINE_ALIAS} 'if [ -f ~/createrole_sql ]; then rm ~/createrole_sql; fi'

echo "uploading sql for schema creation"
rsync -avz -e ssh "${PROVISIONING_SQL_FULLFILENAME}" ${SSH_DOCKERMACHINE_ALIAS}:/home/go
echo "initializing database with new schema..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'psql -h localhost -U '${ROLE_NAME}' -q -d '${DATABASE_NAME}' -f ~/'${PROVISIONING_SQL_FILENAME}''

# clean temporary files locally
if [ -f ~/createrole_sql ]
then
    rm ~/createrole_sql
fi
# clean temporary files remotely
ssh ${SSH_DOCKERMACHINE_ALIAS} 'rm -f ~/'${PROVISIONING_SQL_FILENAME}''

echo " --- listing created tables --- "
ssh docker_pgsql psql '-h localhost -U '${ROLE_NAME}' -d '${DATABASE_NAME}' -c "\dt"'

# PSQL SPECIFIC PART END - provision database with role, schema and tables