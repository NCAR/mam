from fedora:33

RUN dnf -y update \
    && dnf install -y sudo \
    && adduser test_user \
    && echo "test_user ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/test_user \
    && chmod 0440 /etc/sudoers.d/test_user

USER test_user
WORKDIR /home/test_user

RUN sudo dnf -y install \
        gcc-fortran \
        gcc-c++ \
        gcc \
        cmake \
        make \
        netcdf-fortran-devel \
        openmpi-devel \
        git \
        valgrind \
    && sudo dnf clean all

ENV PATH="${PATH}:/usr/lib64/openmpi/bin/"

COPY --chown=test_user . /home/test_user/mam/

RUN cd mam/test/integration/current_cam/pio2 \
    && mkdir build \
    && cd build \
    && CC=mpicc FC=mpif90 cmake -D PIO_ENABLE_EXAMPLES:BOOL=FALSE \
                                -D PIO_ENABLE_TIMING:BOOL=FALSE \
                                -D CMAKE_BUILD_TYPE=DEBUG \
                                .. \
    && sudo make install

RUN mkdir build \
    && cd build \
    && cmake -D CMAKE_C_COMPILER="mpicc" \
             -D CMAKE_Fortran_COMPILER="mpif90" \
             -D CMAKE_BUILD_TYPE=DEBUG \
             ../mam \
    && make
