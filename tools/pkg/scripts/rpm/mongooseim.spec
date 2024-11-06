%undefine __brp_mangle_shebangs
%global __requires_exclude ^\/usr\/bin\/(python|perl)$

Name:              mongooseim
Version:           %{version}
Release:           %{release}
Summary:           A mobile messaging platform with focus on performance and scalability

Group:             Development/Languages
License:           GPLv2
URL:               http://packages.erlang-solutions.com
Packager:          Erlang Solutions Ltd.
Source0:           mongooseim.service


BuildArch:         %{architecture}

BuildRequires:     git
BuildRequires:     make
BuildRequires:     zlib-devel
BuildRequires:     unixODBC-devel
BuildRequires:     gcc
BuildRequires:     gcc-c++
BuildRequires:     openssl
BuildRequires:     openssl-devel
BuildRequires:     systemd-rpm-macros

%description
MongooseIM is Erlang Solutions' robust and efficient XMPP server aimed at large
installations. Specifically designed for enterprise purposes, it is
fault-tolerant, can utilize resources of multiple clustered machines and easily
scale in need of more capacity (by just adding a box/VM).

%prep
%setup -T -D -n mongooseim
cp %{SOURCE0} .

%install
make clean
./tools/configure with-all user=root prefix=/ system=yes
sed -i 's#PREFIX=\"/\"#PREFIX=\"%{buildroot}\"#' configure.out
make install
sed -i 's#RUNNER_USER=\"root\"#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/bin/mongooseimctl
sed -i 's#RUNNER_USER=\"root\"#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/lib/mongooseim/bin/mongooseimctl
sed -i 's#RUNNER_USER=root#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/lib/mongooseim/bin/mongooseim
chrpath -d %{buildroot}/usr/lib/mongooseim/lib/crypto-*/priv/lib/crypto.so
chrpath -d %{buildroot}/usr/lib/mongooseim/lib/crypto-*/priv/lib/otp_test_engine.so

install -p -D -m 0644 %{SOURCE0} %{buildroot}%{_unitdir}/mongooseim.service

%pre
if ! [ -d %{_localstatedir}/lock ]; then
    # /var/lock links to non-existing /run/lock on Rocky Linux 9
    mkdir -m 755 `readlink -f %{_localstatedir}/lock`
fi
getent group mongooseim >/dev/null || groupadd mongooseim
getent passwd mongooseim >/dev/null || adduser -g mongooseim mongooseim
exit 0

%post
echo "MongooseIM %{version} installed"

%clean
rm -rf %{buildroot}

%files

%defattr(-, root, root, -)

%{_bindir}/mongooseimctl
%{_prefix}/lib/mongooseim/*

%{_unitdir}/mongooseim.service

%config	 %{_sysconfdir}/mongooseim/*

%attr(770,mongooseim,mongooseim) %{_prefix}/lib/mongooseim/etc
%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/lib/mongooseim
%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/log/mongooseim
%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/lock/mongooseim

%changelog
* %(date "+%a %b %d %Y") Erlang Solutions <packages@erlang-solutions.com> - %{version}-%{release}
- MongooseIM upstream release
