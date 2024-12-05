executor_arch=$(uname -m)
case "$executor_arch" in
  x86_64)
    expected_arch="amd64"
    ;;
  aarch64)
    expected_arch="arm64"
    ;;
  *)
    echo "Unsupported executor architecture: $executor_arch"
    exit 1
    ;;
esac

echo "Executor architecture detected: $expected_arch"

package_file=$(find tools/pkg/packages -name "*.deb" -o -name "*.rpm")
if [ -z "$package_file" ]; then
    echo "No package found in the output directory."
    exit 1
fi

if [[ $package_file == *.deb ]]; then
    actual_arch=$(dpkg --info "$package_file" | grep Architecture | awk '{print $2}')
elif [[ $package_file == *.rpm ]]; then
  actual_arch=$(rpm -qi "$package_file" | grep Architecture | awk '{print $2}')
  if [ "$actual_arch" == "x86_64" ]; then
    actual_arch="amd64"
  elif [ "$actual_arch" == "aarch64" ]; then
    actual_arch="arm64"
  else
    echo "Unable to determine architecture from file output."
    exit 1
  fi
else
    echo "Unknown package type: $package_file"
    exit 1
fi

if [ "$expected_arch" != "$actual_arch" ]; then
    echo "Architecture mismatch: expected $expected_arch but got $actual_arch"
    exit 1
fi

echo "Package architecture verified: $actual_arch"
