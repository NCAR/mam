mam
===

A modal aerosol model

[![License](https://img.shields.io/github/license/NCAR/mam.svg)](https://github.com/NCAR/mam/blob/main/LICENSE) [![CI Status](https://github.com/NCAR/mam/actions/workflows/tests.yml/badge.svg)](https://github.com/NCAR/mam/actions/workflows/tests.yml) [![codecov](https://codecov.io/gh/NCAR/mam/branch/main/graph/badge.svg?token=WIBA0JE3OE)](https://codecov.io/gh/NCAR/mam)

UNDER DEVELOPMENT - THIS IS NOT YET A FUNCTIONAL AEROSOL MODEL

A working draft of the documentation can be found [here](https://ncar.github.io/mam)

## Install and Run ##

The only requirement to run the code is to have git and to have [Docker Desktop](https://www.docker.com/get-started) installed and running. To build the MAM library and run the tests (functionality is currently limited to aerosol optical property calculations):

```
git clone --recurse-submodules https://github.com/NCAR/mam.git
cd mam/
docker build -t mam-test .
docker run -it mam-test bash
cd build/
make test
```
