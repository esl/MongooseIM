CA_SUB  = /C=PL/ST=Malopolska/L=Krakow/CN=MongooseIM Fake CA
SRV_SUB = /C=PL/ST=Malopolska/L=Krakow/CN=MongooseIM Server

all: ca/cacert.pem fake_cert.pem fake_server.pem fake_dh_server.pem

ca/cacert.pem ca/cakey.pem: openssl-ca.cnf
	mkdir -p ca
	-rm ca/cacert.pem
	-rm ca/cakey.pem
	openssl req -x509 -config $< -newkey rsa:2048 -sha256 -nodes \
		-subj '$(CA_SUB)' -out ca/cacert.pem -outform PEM

fake_cert.csr: openssl-server.cnf
	openssl req -config $< -newkey rsa:2048 -sha256 -nodes -out $@ \
		-subj '$(SRV_SUB)' -outform PEM

fake_cert.pem: fake_cert.csr openssl-ca.cnf ca/cacert.pem ca/cakey.pem ca/index.txt ca/serial.txt
	yes | openssl ca -config openssl-ca.cnf -policy signing_policy \
		-extensions signing_req -out $@ -infiles fake_cert.csr

fake_server.pem: fake_cert.pem fake_key.pem
	cat $^ > $@

fake_dh_server.pem:
	openssl dhparam -outform PEM -out $@ 1024

ca/index.txt:
	touch ca/index.txt

ca/serial.txt:
	echo 01 > $@

clean:
	-rm -f ca/*
	-rm fake_cert.csr fake_cert.pem fake_key.pem fake_server.pem
