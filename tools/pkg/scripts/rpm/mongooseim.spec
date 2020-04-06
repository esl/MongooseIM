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
BuildRequires:     esl-erlang

%description
MongooseIM is Erlang Solutions' robust and efficient XMPP server aimed at large
installations. Specifically designed for enterprise purposes, it is
fault-tolerant, can utilize resources of multiple clustered machines and easily
scale in need of more capacity (by just adding a box/VM).

%prep
%setup -T -D -n mongooseim
cp %{SOURCE0} .

%install
./tools/configure with-all user=root prefix=/ system=yes
sed -i 's#PREFIX=\"/\"#PREFIX=\"%{buildroot}\"#' configure.out
make install
sed -i 's#RUNNER_USER=\"root\"#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/bin/mongooseimctl
sed -i 's#RUNNER_USER=\"root\"#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/lib/mongooseim/bin/mongooseimctl
sed -i 's#RUNNER_USER=root#RUNNER_USER=\"mongooseim\"#' %{buildroot}/usr/lib/mongooseim/bin/mongooseim

# Removed due to not specified python version in shbang
# https://fedoraproject.org/wiki/Changes/Make_ambiguous_python_shebangs_error
rm %{buildroot}/usr/lib/mongooseim/lib/re2-*/c_src/re2/re2/make_unicode_casefold.py
rm %{buildroot}/usr/lib/mongooseim/lib/re2-*/c_src/re2/re2/make_unicode_groups.py
rm %{buildroot}/usr/lib/mongooseim/lib/re2-*/c_src/re2/benchlog/benchplot.py

install -p -D -m 0644 %{SOURCE0} %{buildroot}%{_unitdir}/mongooseim.service

%pre
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

%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/lib/mongooseim
%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/log/mongooseim
%attr(770,mongooseim,mongooseim) %dir %{_localstatedir}/lock/mongooseim

%changelog
* %(date "+%a %b %d %Y") Erlang Solutions <packages@erlang-solutions.com> - %{version}-%{release}
- MongooseIM upstream release
