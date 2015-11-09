#!/bin/bash

# Docker image description. top.
DOCKERFILE_HOME="~/docker-mysql"
# DOCKERFILE="docker_mysql"
INSTANCE_NAME="mongooseim-mysql"
IMAGE_NAME="mongooseimim-mysql-backend"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="docker_mysql"


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
    PROVISIONING_SQL_FILENAME="mysql.sql"
    PROVISIONING_SQL_PATH="apps/ejabberd/priv"
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
echo "starting the container with mysql..."

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker run --name '${INSTANCE_NAME}' -p 3306:3306 -d '${IMAGE_NAME}''

# MYSQL SPECIFIC PART - provision database with role, schema and tables

echo "uploading sql for schema creation"
rsync -avz -e ssh "${PROVISIONING_SQL_FULLFILENAME}" ${SSH_DOCKERMACHINE_ALIAS}:/home/go
echo "initializing database with new schema..."

while :
do
   ssh ${SSH_DOCKERMACHINE_ALIAS} "mysql --host=127.0.0.1 -u ${ROLE_NAME} --password=${ROLE_DB_PASSWORD} ${DATABASE_NAME}  < ~/${PROVISIONING_SQL_FILENAME}"
   if [ $? -eq 0 ]; then echo "provisioning database success"; break; fi
done

# clean temporary files remotely
ssh ${SSH_DOCKERMACHINE_ALIAS} 'rm -f ~/'${PROVISIONING_SQL_FILENAME}''

echo " --- listing created tables --- "
ssh ${SSH_DOCKERMACHINE_ALIAS} 'mysqlshow --host=127.0.0.1 -u '${ROLE_NAME}' '${DATABASE_NAME}' --password='${ROLE_DB_PASSWORD}''

# MYSQL SPECIFIC PART END - provision database with role, schema and tables
