cmake_minimum_required(VERSION 3.12)
project(MAM)
enable_language(Fortran)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

################################################################################
# options

################################################################################
# json-fortran library

find_path(JSON_INCLUDE_DIR json_module.mod
  DOC "json-fortran include directory (must include json_*.mod files)"
  PATHS
    $ENV{JSON_FORTRAN_HOME}/lib
    /opt/local/lib
    /usr/local/lib
    /usr/local/lib64)
find_library(JSON_LIB jsonfortran
  DOC "json-fortran library"
  PATHS
    $ENV{JSON_FORTRAN_HOME}/lib
    /opt/local/lib
    /usr/local/lib
    /usr/local/lib64)
include_directories(${JSON_INCLUDE_DIR})

################################################################################
# NetCDF library

find_path(NETCDF_INCLUDE_DIR netcdf.mod NETCDF.mod
  DOC "NetCDF include directory (must contain netcdf.mod)"
  PATHS
    $ENV{NETCDF_HOME}/include
    /usr/lib/gfortran/modules
    /usr/lib64/gfortran/modules
    /opt/local/include)
find_library(NETCDF_C_LIB netcdf
  DOC "NetCDF C library"
  PATHS
    $ENV{NETCDF_HOME}/lib
    $ENV{NETCDF_HOME}/lib64
    opt/local/lib)
find_library(NETCDF_FORTRAN_LIB netcdff
  DOC "NetCDF Fortran library"
  PATHS
    $ENV{NETCDF_HOME}/lib
    $ENV{NETCDF_HOME}/lib64
    /opt/local/lib)
set(NETCDF_LIBS ${NETCDF_LIB} ${NETCDF_FORTRAN_LIB})
include_directories(${NETCDF_INCLUDE_DIR})

################################################################################
# ParallelIO library

find_path(NETCDF_INCLUDE_DIR netcdf.h
  DOC "NetCDF include directory (must contain netcdf.h)"
  PATHS
    $ENV{NETCDF_HOME}/include
    usr/local/include
    opt/local/include)
find_library(NETCDF_C_LIB netcdf
  DOC "NetCDF C library"
  PATHS
    $ENV{NETCDF_HOME}/lib
    $ENV{NETCDF_HOME}/lib64
    usr/local/lib
    usr/local/lib64
    opt/local/lib
    opt/local/lib64)
find_path(PIO_C_INCLUDE_DIR pio.h
  DOC "ParallelIO C include directory (must contain pio.h)"
  PATHS
    $ENV{PIO_HOME}/include
    usr/local/include
    opt/local/include)
find_library(PIO_C_LIB pioc
  DOC "ParallelIO C library"
  PATHS
  $ENV{PIO_HOME}/lib
  $ENV{PIO_HOME}/lib64
  usr/local/lib
  usr/local/lib64
  opt/local/lib
  opt/local/lib64)
find_path(PIO_FORTRAN_INCLUDE_DIR pio.mod
  DOC "ParallelIO Fortran include directory (must contain pio.mod)"
  PATHS
    $ENV{PIO_HOME}/include
    usr/local/include
    opt/local/include)
find_library(PIO_FORTRAN_LIB piof
  DOC "ParallelIO Fortran library"
  PATHS
    $ENV{PIO_HOME}/lib
    $ENV{PIO_HOME}/lib64
    usr/local/lib
    usr/local/lib64
    opt/local/lib
    opt/local/lib64)
set(PIO_LIBS ${PIO_FORTRAN_LIB} ${PIO_C_LIB} ${NETCDF_C_LIB})
include_directories(${NETCDF_INCLUDE_DIR})
include_directories(${PIO_C_INCLUDE_DIR})
include_directories(${PIO_FORTRAN_INCLUDE_DIR})

################################################################################
# musica-core library

add_subdirectory(libs/aerosol-interface/libs/musica-core/src)
include_directories(${CMAKE_BINARY_DIR}/libs/aerosol-interface/libs/musica-core/src)

################################################################################
# Aerosol-interface library

add_subdirectory(libs/aerosol-interface/src)
include_directories(${CMAKE_BINARY_DIR}/libs/aerosol-interface/src)

################################################################################
# MAM library

add_subdirectory(src)
include_directories(${CMAKE_BINARY_DIR}/src)

################################################################################
# MAM tests

enable_testing()
add_subdirectory(test)

################################################################################
