################################################################################
# Link mamlib to a test and add it to the suite

macro(add_std_test test_name)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} mamlib)
  add_test(NAME ${test_name} COMMAND ${test_name})
endmacro(add_std_test)

################################################################################
# MAM unit tests

add_executable(core core.F90
                    mock_mode.F90
                    mock_interpolation_strategy.F90
                    mock_optics_lookup.F90
                    mock_species.F90
                    ../../src/constants.F90
                    ../../src/core.F90
                    ../../src/optics_util.F90
                    )
target_link_libraries(core musicalib aerosollib)
add_test(NAME core COMMAND core)

add_executable(mode mode.F90
                    mock_interpolation_strategy.F90
                    mock_optics_lookup.F90
                    mock_species.F90
                    ../../src/constants.F90
                    ../../src/mode.F90
                    ../../src/optics_util.F90
                    )
target_link_libraries(mode musicalib aerosollib)
add_test(NAME mode COMMAND mode)

add_executable(species species.F90)
add_std_test(species)

add_executable(optics_lookup optics_lookup.F90)
add_std_test(optics_lookup)

add_executable(optics_util optics_util.F90
                           mock_interpolation_strategy.F90
              )
add_std_test(optics_util)

################################################################################
