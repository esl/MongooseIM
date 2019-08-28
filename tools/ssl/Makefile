all: mongooseim/cert.pem mongooseim/key.pem \
  mongooseim/server.pem mongooseim/dh_server.pem \
  mongooseim/pubkey.pem mongooseim/privkey.pem \
  ca/cacert.pem ca/cakey.pem ca/index.txt ca/serial.txt \
  ca-clients/cacert.pem ca-clients/cakey.pem ca-clients/index.txt ca-clients/serial.txt
	chmod -R a+r mongooseim ca ca-clients

%/cacert.pem %/cakey.pem: openssl-%.cnf
	mkdir -p $(@D)
	openssl req -x509 -config $< -newkey rsa:2048 -sha256 -nodes \
	            -out $(@D)/cacert.pem -keyout $(@D)/cakey.pem -outform PEM

%/cert.csr %/key.pem: openssl-%.cnf
	mkdir -p $(@D)
	openssl req -config $< -newkey rsa:2048 -sha256 -nodes \
		-out $(@D)/cert.csr -keyout $(@D)/key.pem \
		-outform PEM

%/cert.pem: %/cert.csr openssl-ca.cnf ca/cacert.pem ca/cakey.pem | ca/index.txt ca/serial.txt
	yes | openssl ca -config openssl-ca.cnf -policy signing_policy \
		-extensions signing_req -out $@ -infiles $<

%/server.pem: %/cert.pem %/key.pem
	cat $^ > $@

%/pubkey.pem: %/cert.pem
	openssl x509 -pubkey -noout -in $< > $@

%/privkey.pem: %/key.pem
	openssl rsa -in $< -out $@

# About dsaparam argument
# It speeds up generation of dhparam
# Speed is useful for fake certificates
# https://security.stackexchange.com/questions/95178/diffie-hellman-parameters-still-calculating-after-24-hours
#
# Certs generated with -dsaparam don't work with our slapd container though.
# So, let's remove the option for now.
# https://bugs.launchpad.net/ubuntu/+source/openldap/+bug/1724285
%/dh_server.pem:
	openssl dhparam -outform PEM -out $@ 1024

%/index.txt:
	mkdir -p $(@D)
	touch $@

%/serial.txt:
	mkdir -p $(@D)
	echo 01 > $@

clean:
	rm -rf ca ca-clients mongooseim
