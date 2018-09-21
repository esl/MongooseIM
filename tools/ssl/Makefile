all: mongooseim/server.pem mongooseim/cert.pem \
  mongooseim/key.pem mongooseim/dh_server.pem \
  mongooseim/pubkey.pem mongooseim/privkey.pem \
  ca/cacert.pem ca/cakey.pem \
  ca-clients/cacert.pem ca-clients/cakey.pem

%/cacert.pem: openssl-%.cnf
	mkdir -p $(@D)
	openssl req -x509 -config $< -newkey rsa:2048 -sha256 -nodes -out $@ -outform PEM

%/cakey.pem: %/cacert.pem;

%/cert.csr: openssl-%.cnf
	mkdir -p $(@D)
	openssl req -config $< -newkey rsa:2048 -sha256 -nodes \
		-out $@ -keyout $(subst cert.csr,key.pem,$@) \
		-outform PEM

%/cert.pem: %/cert.csr openssl-ca.cnf ca/cacert.pem ca/cakey.pem | ca/index.txt ca/serial.txt
	yes | openssl ca -config openssl-ca.cnf -policy signing_policy \
		-extensions signing_req -out $@ -infiles $<

%/key.pem: %/cert.pem;

%/server.pem: %/cert.pem %/key.pem
	cat $^ > $@

%/pubkey.pem: %/cert.pem
	openssl x509 -pubkey -noout -in $< > $@

%/privkey.pem: %/key.pem
	openssl rsa -in $< -out $@

%/dh_server.pem:
	openssl dhparam -outform PEM -out $@ 1024

ca/index.txt:
	mkdir -p $(@D)
	touch $@

ca/serial.txt:
	mkdir -p $(@D)
	echo 01 > $@

clean:
	rm -rf ca ca-clients mongooseim
