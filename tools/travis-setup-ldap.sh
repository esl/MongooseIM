#!/bin/sh

LDAP_P=`cat /tmp/travis_db_password`

LDAP_ROOT="cn=Manager,dc=ejd,dc=com"

cat > manager.ldif << EOL
dn:  olcDatabase={1}hdb,cn=config
changetype: modify
replace: olcSuffix
olcSuffix: dc=ejd,dc=com
-
replace: olcRootDN
olcRootDN: ${LDAP_ROOT}
-
replace: olcRootPW
olcRootPW: ${LDAP_P}
EOL

sudo ldapmodify -Y EXTERNAL -H ldapi:/// -f manager.ldif

cat > init_entries.ldif << EOL
dn: dc=ejd,dc=com
dc: ejd
objectClass: dcObject
objectClass: organizationalUnit
ou: ejd

dn: ou=Users,dc=ejd,dc=com
objectClass: organizationalUnit
ou: users
EOL

ldapadd -x -D${LDAP_ROOT} -f init_entries.ldif -w ${LDAP_P}
