# syntax=docker/dockerfile:latest

FROM scientificlinux/sl:7

ARG cmake=3.23.1

WORKDIR /tmp/

RUN yum install -y gcc gcc-c++ libstdc++ make
RUN yum install -y perl-core perl-libs
RUN yum install -y libX11-devel libXext-devel libXft-devel libXpm-devel libxml2-devel openmotif-devel zlib-devel libffi-devel
RUN yum install -y autoconf automake libtool
RUN yum install -y boost-devel popt-devel

RUN curl -sL https://github.com/Kitware/CMake/releases/download/v${cmake}/cmake-${cmake}-linux-x86_64.tar.gz | tar -xz --strip-components 1 -C /usr

RUN curl -L http://cpanmin.us | perl - --self-upgrade
RUN cpanm Text::Template

WORKDIR /tmp/openssl

RUN curl -k -sL https://www.openssl.org/source/openssl-1.1.1u.tar.gz | tar -xz --strip-components 1
RUN ./config --prefix=/usr/local/custom-openssl --libdir=lib --openssldir=/etc/pki/tls
RUN make -j1 depend
RUN make -j $(nproc)
RUN make install_sw

WORKDIR /tmp/Python-3.8.0

RUN curl -sL https://www.python.org/ftp/python/3.8.0/Python-3.8.0.tgz | tar -xz --strip-components 1
RUN LDFLAGS="${LDFLAGS} -Wl,-rpath=/usr/local/custom-openssl/lib" ./configure --with-openssl=/usr/local/custom-openssl
RUN make -j $(nproc)
RUN make install
RUN pip3 install --upgrade pip

RUN yum install -y gsl-devel
RUN yum install -y giflib-devel
RUN yum install -y libtiff-devel
RUN yum install -y mesa-libGL-devel mesa-libGLU-devel
RUN yum install -y vim

WORKDIR /tmp/root_v5.34.38
RUN curl -sL https://root.cern/download/root_v5.34.38.source.tar.gz | tar -xz --strip-components 1

WORKDIR /tmp/build_root
RUN cmake -S /tmp/root_v5.34.38 -B /tmp/build_root -Dpython=OFF -DCMAKE_INSTALL_PREFIX=/usr/local
RUN cmake --build /tmp/build_root --parallel $(nproc) && cmake --install /tmp/build_root

RUN yum install -y lzo lzo-devel
RUN yum install -y gcc-gfortran

RUN yum install -y git unzip make patch \
    perl perl-Data-Dumper \
    lapack-static blas-static imake motif-devel

RUN yum install -y unixODBC-devel
RUN yum install -y postgresql
RUN yum install -y postgresql-odbc

RUN yum install -y epel-release
RUN yum install -y HepMC-devel

# Install cernlib
WORKDIR /cern
RUN curl -sL https://github.com/psilib/cernlib/archive/9d59c54d.tar.gz | tar -xz --strip-components 1 \
 && ./build_cernlib.sh \
 && mv /usr/lib64/libblas.a   /cern/2006/lib/libblas.a \
 && mv /usr/lib64/liblapack.a /cern/2006/lib/liblapack3.a \
 && ln -s 2006 /cern/pro \
 && rm -fr /cern/2006/src /cern/2006/build

ENV CERN=/cern
ENV CERN_LEVEL=pro
ENV CERN_ROOT=$CERN/$CERN_LEVEL
ENV PATH=$CERN_ROOT/bin:.:$PATH

SHELL ["/bin/bash", "-l", "-c"]

RUN echo "source /usr/local/bin/thisroot.sh" > /etc/profile.d/z10_source_thisroot.sh

WORKDIR /phenix-sw

COPY offline ./offline
COPY online_distribution ./online_distribution
COPY simulation ./simulation
COPY utils ./utils

RUN cp offline/opt_phenix_scripts/bin/phenix_cernlib.pl /bin/

RUN cd /phenix-sw/utils/libodbc++/ && touch aclocal.m4 configure Makefile.am Makefile.in
RUN cd /phenix-sw/utils/libodbc++/ && configure && make -j $(nproc) && make install
RUN cd /phenix-sw/utils/libodbc++/ && configure --prefix=/usr/local/libodbc++ && make -j $(nproc) && make install
RUN cd /phenix-sw/utils/RDBC/ && autogen.sh --with-odbc=/usr/local/libodbc++ && make -j $(nproc) && make install

RUN cd /phenix-sw && utils/rebuild/build.pl --source $PWD --workdir /tmp/build/phenix

COPY --chmod=0755 <<-"EOF" /opt/entrypoint.sh
#!/bin/bash
set -e
exec "$@"
EOF


ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["/bin/bash", "-l"]
