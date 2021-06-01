from fedora:33

RUN dnf -y update \
    && dnf -y install \
        gcc-fortran \
        gcc-c++ \
        gcc \
        cmake \
        make \
    && dnf clean all

COPY . /mam/

RUN mkdir /build \
    && cd /build \
    && cmake /mam \
    && make
