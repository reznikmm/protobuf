%undefine _hardened_build
%define _gprdir %_GNAT_project_dir

Name:       ada-protobuf
Version:    1.1.0
Release:    git%{?dist}
Summary:    An Ada Google Buffers implementation
Group:      Development/Libraries
License:    MIT
URL:        https://github.com/reznikmm/protobuf
### Direct download is not available
Source0:    protobuf.tar.gz
BuildRequires:   gcc-gnat
BuildRequires:   fedora-gnat-project-common  >= 3 
BuildRequires:   matreshka-devel
BuildRequires:   ada-pretty-devel
BuildRequires:   gprbuild
# For tests:
BuildRequires:   protobuf-devel
BuildRequires:   autoconf
BuildRequires:   automake
BuildRequires:   libtool
BuildRequires:   gcc-c++
BuildRequires:   make

Requires:   %{name}-runtime%{?_isa} = %{version}-%{release}

# gprbuild only available on these:
ExclusiveArch: %GPRbuild_arches

%description
An Ada Google Buffers implementation.

This project includes a plugin for `protoc` tool.

%package %{name}-runtime
Summary:    Runtime library for Ada Protobuf
License:    MIT
Group:      System Environment/Libraries

%description %{name}-runtime
Runtime library for Ada Protobuf includes common types
and routines.

%package %{name}-runtime-devel
Summary:    Runtime library for Ada Protobuf
License:    MIT
Group:      Development/Libraries
Requires:   %{name}-runtime%{?_isa} = %{version}-%{release}
Requires:   fedora-gnat-project-common  >= 2

%description %{name}-runtime-devel
%{summary}

%prep
%setup -q -n protobuf

%build
# This package triggers a fault in the Ada compiler when -gnatn is enabled (gcc 11 fc34).
make  %{?_smp_mflags} GPRBUILD_FLAGS="`echo %Gnatmake_optflags|sed -e s/-gnatn.//`"

%check
## find libs without RPATH, Fedora specific
export LD_LIBRARY_PATH="%{buildroot}/%{_libdir}/:$LD_LIBRARY_PATH"
make %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags" check
## Delete compiled python files 
rm -f %{buildroot}/%{_datadir}/gdb/python/%{name}/protobuf.py?

%install
rm -rf %{buildroot}
sed -i -e /Artifacts/s@bin@%{buildroot}%{_bindir}@ gnat/protoc_gen_ada.gpr
make install DESTDIR=%{buildroot} LIBDIR=%{_libdir} PREFIX=%{_prefix} GPRDIR=%{_gprdir} BINDIR=%{_bindir}

%post     -p /sbin/ldconfig
%postun   -p /sbin/ldconfig

%files
%doc LICENSE
%{_bindir}/protoc-gen-ada

%files %{name}-runtime
%dir %{_libdir}/%{name}
%{_libdir}/%{name}/libadapbrt.so.%{version}-git
%{_libdir}/libadapbrt.so.%{version}-git

%files %{name}-runtime-devel
%doc README.md
%{_libdir}/%{name}/libadapbrt.so
%{_libdir}/libadapbrt.so
%{_libdir}/%{name}/*.ali
%{_includedir}/%{name}
%{_datadir}/gdb/python/%{name}
%{_gprdir}/protobuf_runtime.gpr
%{_gprdir}/manifests/protobuf_runtime
%{_gprdir}/manifests/protoc_gen_ada


%changelog
* Wed Feb 15 2023 Manuel Gomez <mgrojo@gmail.com> - 1.1.0-git
- Align to version and library filename used by the Alire crate
* Thu May  7 2020 Maxim Reznik <reznikmm@gmail.com> - 0.1.0-git
- Initial package
