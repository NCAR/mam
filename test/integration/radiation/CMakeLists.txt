################################################################################
# Mock radiation model test

add_executable(test_mock_radiation mock_host.F90
                                   mock_radiation.F90
              )

target_link_libraries(test_mock_radiation mamlib)

add_test(NAME mock_radiation COMMAND run_mock_radiation.sh)

################################################################################
