################################################################################
# Link mamlib to a test and add it to the suite

macro(add_std_test test_name)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} mamlib)
  add_test(NAME ${test_name} COMMAND ${test_name})
endmacro(add_std_test)

################################################################################
# MAM unit tests

add_executable(optics_lookup optics_lookup.F90)
add_std_test(optics_lookup)

################################################################################
