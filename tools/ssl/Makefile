CA_SUB  = /C=PL/ST=Malopolska/L=Krakow/CN=MongooseIM Fake CA
SRV_SUB = /C=PL/ST=Malopolska/L=Krakow/CN=MongooseIM Server

all: ca/cacert.pem fake_cert.pem fake_pubkey.pem fake_privkey.pem fake_server.pem fake_dh_server.pem alice_cert.pem bob_cert.pem

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

fake_pubkey.pem: fake_cert.pem
	openssl x509 -pubkey -noout -in fake_cert.pem > fake_pubkey.pem

fake_privkey.pem: fake_key.pem
	openssl rsa -in fake_key.pem -out fake_privkey.pem

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
	-rm alice_key.pem alice_csr.pem alice_cert.pem
	-rm bob_key.pem bob_csr.pem bob_cert.pem
	-rm fake_dh_server.pem fake_privkey.pem fake_pubkey.pem


# Based on article
# https://medium.com/@sevcsik/authentication-using-https-client-certificates-3c9d270e8326
# Alice has valid cert.
# Bob has self-signed cert (invalid).


# Create a key and a Certificate Signing Request for Alice
alice_key.pem:
	openssl req -newkey rsa:4096 -keyout alice_key.pem \
		-out alice_csr.pem -nodes -days 365 -subj "/CN=Alice"

# We sign Alice’s CSR with our key and save it as a certificate.
# Here, we act as a Certificate Authority,
# so we supply our certificate and key via the -CA parameters
alice_cert.pem: alice_key.pem ca/cacert.pem
	openssl x509 -req -in alice_csr.pem -CA ca/cacert.pem -CAkey ca/cakey.pem \
		-out alice_cert.pem -set_serial 01 -days 365


# Create a key and a Certificate Signing Request for Bob
bob_key.pem:
	openssl req -newkey rsa:4096 -keyout bob_key.pem \
		-out bob_csr.pem -nodes -days 365 -subj "/CN=Bob"

# Bob just signs his certificate on his own, without CA:
bob_cert.pem: bob_key.pem ca/cacert.pem
	openssl x509 -req -in bob_csr.pem -signkey bob_key.pem \
		-out bob_cert.pem -days 365
