#!/usr/bin/env bash
set -e

trap 'rm -f ~/.rpmmacros' EXIT

PACKAGE_NAME=$(find . -maxdepth 1 -type f \( -name "*.deb" -o -name "*.rpm" \))

echo "$GPG_PRIVATE_KEY" | base64 -d | gpg --batch --pinentry-mode loopback --import

GPG_KEY_ID=$(gpg --list-keys --with-colons | grep '^pub' | cut -d':' -f5)
if [ -z "$GPG_KEY_ID" ]; then
    echo "Error: Failed to import GPG key."
    exit 1
fi

GPG_KEY_EMAIL=$(gpg --list-keys --with-colons | grep '^uid' | cut -d':' -f10 | head -n 1)

echo "$GPG_PUBLIC_KEY" | base64 -d > public.key

if [[ "$PACKAGE_NAME" == *.deb ]]; then
    gpg --import public.key
    rm -f public.key

    debsigs --gpgopts "--no-tty --pinentry-mode loopback --passphrase $GPG_PASS" \
            --sign=origin -k="$GPG_KEY_ID" "$PACKAGE_NAME"
    echo "DEB package signed successfully: $PACKAGE_NAME"

    debsigs --verify "$PACKAGE_NAME"
    echo "DEB package verified successfully: $PACKAGE_NAME"
elif [[ "$PACKAGE_NAME" == *.rpm ]]; then
    rpm --import public.key
    rm -f public.key

    cat > ~/.rpmmacros <<EOF
          %__gpg $(type -p gpg)
          %_gpg_path $HOME/.gnupg
          %_gpg_name $GPG_KEY_EMAIL
          %_signature gpg
          %_gpg_pass $GPG_PASS
          %__gpg_sign_cmd %{__gpg} gpg --no-verbose --no-armor --batch \
            --pinentry-mode loopback --passphrase "%{_gpg_pass}" \
            --no-secmem-warning -u "%{_gpg_name}" \
            -sbo %{__signature_filename} %{__plaintext_filename}
EOF

    rpm --addsign "$PACKAGE_NAME"
    echo "RPM package signed successfully: $PACKAGE_NAME"

    rpm --checksig "$PACKAGE_NAME"
    echo "RPM package verified successfully: $PACKAGE_NAME"

    rm -f ~/.rpmmacros
else
    echo "No packages found to sign"
    exit 1
fi
