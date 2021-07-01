################################################################################
# Lookup table 2D 1D test

add_executable(test_lookup_table_2D_1D  test_lookup_table_2D_1D.F90
              )

target_link_libraries(test_lookup_table_2D_1D mamlib ${NETCDF_LIBS})

add_test(NAME test_lookup_table_2D_1D COMMAND run_lookup_table_2D_1D.sh)

################################################################################
