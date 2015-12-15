#!/bin/bash

DOCKERFILE_HOME="~/docker-pgsql"
# DOCKERFILE="docker_pgsql"

INSTANCE_NAME="mongooseim-postgre"
IMAGE_NAME="astachurski/docker-pgsql"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="backendhost"

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
    # start script from pipeline root folder!
    PROVISIONING_SQL_PATH="apps/ejabberd/priv"
fi

PROVISIONING_SQL_FULLFILENAME=${PROVISIONING_SQL_PATH}/${PROVISIONING_SQL_FILENAME}

echo "Provisioning SQL fullname is:" ${PROVISIONING_SQL_FULLFILENAME}

echo "trying to stop docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker stop '${INSTANCE_NAME}''
echo "trying to remove docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker rm '${INSTANCE_NAME}''
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
    sleep 1
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
ssh ${SSH_DOCKERMACHINE_ALIAS} 'psql -h localhost -U '${ROLE_NAME}' -d '${DATABASE_NAME}' -c "\dt"'

# PSQL SPECIFIC PART END - provision database with role, schema and tables
