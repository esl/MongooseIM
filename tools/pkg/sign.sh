#!/usr/bin/env bash
set -e

cd tools/pkg/packages
PACKAGE_NAME=$(ls)

GPG_KEY_NAME="MongooseIM"
GPG_KEY_EMAIL="mongooseim@erlang-solutions.com"

if [ -z "$GPG_PASS" ]; then
    echo "Error: GPG_PASS environment variable is not set."
    exit 1
fi

GPG_KEY_ID=$(gpg --list-keys --with-colons "$GPG_KEY_EMAIL" 2>/dev/null | grep '^pub' | cut -d':' -f5)

if [ -z "$GPG_KEY_ID" ]; then
    GPG_BATCH_FILE=$(mktemp)
    cat > "$GPG_BATCH_FILE" <<EOF
        %echo Generating a basic OpenPGP key
        Key-Type: default
        Subkey-Type: default
        Key-Curve: Ed25519
        Subkey-Curve: Ed25519
        Name-Real: $GPG_KEY_NAME
        Name-Email: $GPG_KEY_EMAIL
        Expire-Date: 1y
        %commit
        %echo Done
EOF

    gpg --batch --passphrase "$GPG_PASS" --pinentry-mode loopback --generate-key "$GPG_BATCH_FILE"
    rm -f "$GPG_BATCH_FILE"

    GPG_KEY_ID=$(gpg --list-keys --with-colons "$GPG_KEY_EMAIL" | grep '^pub' | cut -d':' -f5)
fi

if [[ "$PACKAGE_NAME" == *.deb ]]; then
    echo "Signing DEB package: $PACKAGE_NAME"

    dpkg-sig --sign builder -g "--no-tty --pinentry-mode loopback --passphrase $GPG_PASS" \
         -k "$GPG_KEY_ID" \
         $PACKAGE_NAME

    # Verify the signature
    dpkg-sig --verify "$PACKAGE_NAME"

    echo "DEB package signed successfully: $PACKAGE_NAME"

elif [[ "$PACKAGE_NAME" == *.rpm ]]; then
    echo "Signing RPM package: $PACKAGE_NAME"

    gpg --export -a "$GPG_KEY_ID" > public.key
    rpm --import public.key
    rm -f public.key

    # Configure RPM macros
    cat > ~/.rpmmacros <<EOF
          %__gpg $(which gpg)
          %_gpg_path $HOME/.gnupg
          %_gpg_name $GPG_KEY_EMAIL
          %_signature gpg
          %_gpg_pass $GPG_PASS
          %__gpg_sign_cmd %{__gpg} gpg --no-verbose --no-armor --batch \
            --pinentry-mode loopback --passphrase "%{_gpg_pass}" \
            --no-secmem-warning -u "%{_gpg_name}" \
            -sbo %{__signature_filename} %{__plaintext_filename}
EOF

    echo "Signing the RPM package..."
    rpm --addsign "$PACKAGE_NAME"

    # Verify the signature
    rpm --checksig "$PACKAGE_NAME"

    echo "RPM package signed successfully: $PACKAGE_NAME"

else
    echo "Unknown package type: $PACKAGE_NAME"
    exit 1
fi

echo "Package signing process completed."
