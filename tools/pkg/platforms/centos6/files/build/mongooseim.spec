%undefine _missing_build_ids_terminate_build
%define _libdir /usr/lib/mongooseim
%define _etcdir /etc
%define _bindir /usr/bin
%define _vardir /var

Summary: %{_summary}
Name: %{_name}
Version: %{_vers}
Release: %{_release}
Source: %{name}_%{version}.tar.gz
License: %{_license}
Group: "Development/Languages"
BuildArch: @ARCH@
BuildRoot: %{_topdir}/BUILDROOT/%{name}-%{version}.@ARCH@
BuildRequires:  esl-erlang <= 18.3
BuildRequires:  expat-devel
BuildRequires:	chrpath
Requires(pre): shadow-utils

Vendor: %{_vendor}

%description
MongooseIM is Erlang Solutions' robust and efficient XMPP server aimed at large
installations. Specifically designed for enterprise purposes, it is 
fault-tolerant, can utilize resources of multiple clustered machines and easily 
scale in need of more capacity (by just adding a box/VM).

%pre 
getent group mongooseim >/dev/null || groupadd mongooseim
getent passwd mongooseim >/dev/null || adduser -g mongooseim mongooseim
exit 0

%post
echo "MongooseIM %{version} installed"

%clean
echo "Cleaned"

%files
%defattr(-,mongooseim,mongooseim,-)
%{_libdir}/*
%{_bindir}/*
%{_etcdir}/*
%{_vardir}/lib/mongooseim
%{_vardir}/log/mongooseim
%{_vardir}/lock/mongooseim

