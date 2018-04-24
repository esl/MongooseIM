#!/bin/bash

MYSQLDIR=/var/lib/mysql

echo "Preparing SSL certificates"
cp ${SQL_TEMP_DIR}/fake_cert.pem ${MYSQLDIR}/.
cp ${SQL_TEMP_DIR}/fake_key.pem ${MYSQLDIR}/.
chown mysql:mysql ${MYSQLDIR}/fake*
chmod 444 ${MYSQLDIR}/fake_cert.pem
chmod 400 ${MYSQLDIR}/fake_key.pem

echo 'Configuring mysql'
mysql -u root -h localhost --password=${MYSQL_ROOT_PASSWORD} -e \
      "GRANT USAGE ON *.* TO '${MYSQL_USER}'@'%'; \
       ALTER USER '${MYSQL_USER}'@'%' REQUIRE SSL;"
