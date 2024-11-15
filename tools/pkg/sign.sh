#!/usr/bin/env bash
set -e

cd tools/pkg/packages
PACKAGE_NAME=$(ls)

GPG_KEY_NAME="Test User"
GPG_KEY_EMAIL="testuser@example.com"

GPG_KEY_ID=$(gpg --list-keys --with-colons "$GPG_KEY_EMAIL" 2>/dev/null | grep '^pub' | cut -d':' -f5)

if [ -z "$GPG_KEY_ID" ]; then
    GPG_BATCH_FILE=$(mktemp)
    cat > "$GPG_BATCH_FILE" <<EOF
        %no-protection
        %echo Generating a basic OpenPGP key
        Key-Type: default
        Subkey-Type: default
        Key-Curve: Ed25519
        Subkey-Curve: Ed25519
        Name-Real: $GPG_KEY_NAME
        Name-Email: $GPG_KEY_EMAIL
        Expire-Date: 0
        %commit
        %echo Done
EOF

    gpg --batch --generate-key "$GPG_BATCH_FILE"
    rm -f "$GPG_BATCH_FILE"

    GPG_KEY_ID=$(gpg --list-keys --with-colons "$GPG_KEY_EMAIL" | grep '^pub' | cut -d':' -f5)
fi

if [[ "$PACKAGE_NAME" == *.deb ]]; then
    echo "Signing DEB package: $PACKAGE_NAME"

    dpkg-sig --sign builder \
        -k "$GPG_KEY_ID" \
        --gpg-options "--batch --yes --pinentry-mode loopback" \
        "$PACKAGE_NAME"

    # Verify the signature
    dpkg-sig --verify "$PACKAGE_NAME"

    echo "DEB package signed successfully: $PACKAGE_NAME"

elif [[ "$PACKAGE_NAME" == *.rpm ]]; then
    echo "Signing RPM package: $PACKAGE_NAME"

    gpg --export -a "$GPG_KEY_ID" > public.key
    rpm --import public.key
    rm -f public.key

    echo "%__gpg $(which gpg)" >> ~/.rpmmacros
    echo "%_gpg_path $HOME/.gnupg" >> ~/.rpmmacros
    echo "%_gpg_name $GPG_KEY_EMAIL" >> ~/.rpmmacros
    echo "%_signature gpg" >> ~/.rpmmacros

    rpm --addsign "$PACKAGE_NAME"

    # Verify the signature
    rpm --checksig "$PACKAGE_NAME"

    echo "RPM package signed successfully: $PACKAGE_NAME"

else
    echo "Unknown package type: $PACKAGE_NAME"
    exit 1
fi

rm -rf ~/.gnupg

echo "Package signing process completed."
