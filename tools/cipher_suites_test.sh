#!/usr/bin/env bash

# OpenSSL requires the port number.
SERVER=localhost:5222
DELAY=1
ciphers=$(openssl ciphers 'ALL:eNULL' | sed -e 's/:/ /g')

echo Obtaining cipher list from $(openssl version).

for cipher in ${ciphers[@]}
	#for cipher in ADH-AES256-SHA
do
	echo -n Testing $cipher...
	result=$(echo -n | openssl s_client -cipher "$cipher" -connect $SERVER -starttls xmpp 2>&1)
	if [[ "$result" =~ "Cipher is ${cipher}" ]] ; then
		echo YES
	else
		#echo $result
		if [[ "$result" =~ ":error:" ]] ; then
			error=$(echo -n $result | cut -d':' -f6)
			echo NO \($error\)
		else
			echo UNKNOWN RESPONSE
			echo $result
		fi
	fi
	sleep $DELAY
done
