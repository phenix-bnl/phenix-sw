# syntax=docker/dockerfile:latest

FROM scientificlinux/sl:7

ARG cmake=3.23.1

RUN yum install -y gcc gcc-c++ libstdc++ make
RUN yum install -y perl-core perl-libs
RUN yum install -y libX11-devel libXext-devel libXft-devel libXpm-devel libxml2-devel openmotif-devel zlib-devel libffi-devel
RUN yum install -y autoconf automake libtool
RUN yum install -y boost-devel popt-devel

RUN curl -sL https://github.com/Kitware/CMake/releases/download/v${cmake}/cmake-${cmake}-linux-x86_64.tar.gz | tar -xz --strip-components 1 -C /usr

RUN curl -L http://cpanmin.us | perl - --self-upgrade
RUN cpanm Text::Template

WORKDIR /usr/local/opt/openssl
RUN curl -k -sL https://www.openssl.org/source/openssl-1.1.1u.tar.gz | tar -xz --strip-components 1
RUN ./config --prefix=/usr/local/custom-openssl --libdir=lib --openssldir=/etc/pki/tls
RUN make -j1 depend
RUN make -j $(nproc)
RUN make install_sw

WORKDIR /usr/local/opt/Python-3.8.0
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
RUN yum install -y rsync
RUN yum install -y lzo lzo-devel
RUN yum install -y gcc-gfortran

RUN yum install -y unixODBC-devel
RUN yum install -y postgresql
RUN yum install -y postgresql-odbc

RUN yum install -y epel-release
#RUN yum install -y HepMC-devel

RUN yum install -y git unzip make patch \
    perl perl-Data-Dumper \
    lapack-static blas-static imake motif-devel

RUN yum install -y binutils-devel

WORKDIR /usr/local/opt/pythia8245
RUN curl -sL https://pythia.org/download/pythia82/pythia8245.tgz | tar -xz --strip-components 1
RUN ./configure --prefix=/usr/local && make -j $(nproc) && make install

WORKDIR /usr/local/opt/pythia6
RUN curl -sL https://root.cern.ch/download/pythia6.tar.gz | tar -xz --strip-components 1
RUN rm pythia*.f
RUN curl -sLO https://pythia.org/download/pythia6/pythia6428.f
RUN ./makePythia6.linuxx8664
RUN mkdir -p /usr/local/lib/pythia6 && cp libPythia6.so /usr/local/lib/pythia6/ && cp libPythia6.so /usr/local/lib/

WORKDIR /usr/local/opt/root_v5.34.38
RUN curl -sL https://root.cern/download/root_v5.34.38.source.tar.gz | tar -xz --strip-components 1
RUN sed -i 's/LIBRARIES Core /LIBRARIES Core dl /g' /usr/local/opt/root_v5.34.38/montecarlo/pythia8/CMakeLists.txt
WORKDIR /usr/local/opt/build/root
RUN cmake -S /usr/local/opt/root_v5.34.38 -B /usr/local/opt/build/root -Dpython=OFF -Dminuit2=ON -DCMAKE_INSTALL_PREFIX=/usr/local
RUN cmake --build /usr/local/opt/build/root --parallel $(nproc) && cmake --install /usr/local/opt/build/root

WORKDIR /usr/local/opt/hdf5
RUN curl -sL https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_10_11.tar.gz | tar -xz --strip-components 1
WORKDIR /usr/local/opt/build/hdf5
RUN cmake -S /usr/local/opt/hdf5 -B /usr/local/opt/build/hdf5 -DCMAKE_INSTALL_PREFIX=/usr/local -DHDF5_BUILD_CPP_LIB=ON
RUN cmake --build /usr/local/opt/build/hdf5 --parallel $(nproc) && cmake --install /usr/local/opt/build/hdf5

WORKDIR /usr/local/opt/clhep
RUN curl -sL https://proj-clhep.web.cern.ch/proj-clhep/dist1/clhep-2.3.2.2.tgz | tar -xz --strip-components 2
WORKDIR /usr/local/opt/build/clhep
RUN cmake -S /usr/local/opt/clhep -B /usr/local/opt/build/clhep/
RUN cmake --build /usr/local/opt/build/clhep --parallel $(nproc) && cmake --install /usr/local/opt/build/clhep

WORKDIR /usr/local/opt/hepmc
RUN curl -sL https://gitlab.cern.ch/hepmc/HepMC/-/archive/HEPMC_02_06_08/HepMC-HEPMC_02_06_09.tar.gz | tar -xz --strip-components 1
WORKDIR /usr/local/opt/build/hepmc
RUN cmake -S /usr/local/opt/hepmc -B /usr/local/opt/build/hepmc/ -Dmomentum=GEV -Dlength=CM -DCMAKE_INSTALL_PREFIX=/usr/local/opt/build/phenix/new/install.1/
RUN cmake --build /usr/local/opt/build/hepmc --parallel $(nproc) && cmake --install /usr/local/opt/build/hepmc/

# Install cernlib
WORKDIR /cern
RUN curl -sL https://github.com/psilib/cernlib/archive/9d59c54d.tar.gz | tar -xz --strip-components 1 \
 && ./build_cernlib.sh \
 && mv /usr/lib64/libblas.a   /cern/2006/lib/libblas.a \
 && mv /usr/lib64/liblapack.a /cern/2006/lib/liblapack3.a \
 && ln -s 2006 /cern/pro \
 && rm -fr /cern/2006/build

ENV CERN=/cern
ENV CERN_LEVEL=pro
ENV CERN_ROOT=$CERN/$CERN_LEVEL
ENV PATH=$CERN_ROOT/bin:.:$PATH

SHELL ["/bin/bash", "-l", "-c"]

RUN echo "source /usr/local/bin/thisroot.sh" > /etc/profile.d/z10_source_thisroot.sh

WORKDIR /phenix-sw

COPY event_gen ./event_gen
COPY generators ./generators
COPY offline ./offline
COPY online ./online
COPY online_distribution ./online_distribution
COPY simulation ./simulation
COPY utils ./utils

RUN cp offline/opt_phenix_scripts/bin/phenix_cernlib.pl /bin/

RUN cd /phenix-sw/utils/libodbc++/ && touch aclocal.m4 configure Makefile.am Makefile.in
RUN cd /phenix-sw/utils/libodbc++/ && configure && make -j $(nproc) && make install
RUN cd /phenix-sw/utils/libodbc++/ && configure --prefix=/usr/local/libodbc++ && make -j $(nproc) && make install
RUN cd /phenix-sw/utils/RDBC/ && autogen.sh --with-odbc=/usr/local/libodbc++ && make -j $(nproc) && make install

RUN utils/rebuild/build.pl --source $PWD --workdir /usr/local/opt/build/phenix 2>&1 | tee build.log

RUN mkdir offline/AnalysisTrain/Run16dAuPi0Photon/build \
 && cd offline/AnalysisTrain/Run16dAuPi0Photon/build \
 && ../autogen.sh --prefix=/usr/local/opt/build/phenix/new/install/ \
 && make -j $(nproc) \
 && make install

ENV OFFLINE_MAIN=/usr/local/opt/build/phenix/new/install/
ENV LD_LIBRARY_PATH=/usr/local/opt/build/phenix/new/install/lib/:$LD_LIBRARY_PATH
ENV TSEARCHPATH=.:/usr/local/opt/build/phenix/new/install/
ENV GSEARCHPATH=.:PG

COPY odbc.ini ./odbc.ini

COPY --chmod=0755 <<-"EOF" /opt/entrypoint.sh
#!/bin/bash
set -e
exec "$@"
EOF


ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["/bin/bash", "-l"]
