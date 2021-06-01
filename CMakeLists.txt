cmake_minimum_required(VERSION 3.12)
project(MUSICA)
enable_language(Fortran)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

################################################################################
# options

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
