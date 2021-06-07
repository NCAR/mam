################################################################################
# Mock radiation model test

add_definitions(-DSPMD)
add_executable(test_current_cam  aer_rad_props.F90
                                 cam_abortutils_stubs.F90
                                 cam_history_stubs.F90
                                 cam_history_support_stubs.F90
                                 cam_instance_stubs.F90
                                 cam_logfile_stubs.F90
                                 cam_map_utils.F90
                                 cam_pio_utils.F90
                                 constituents_stubs.F90
                                 ioFileMod.F90
                                 mock_cam.F90
                                 modal_aer_opt.F90
                                 modal_aero_calcsize_stubs.F90
                                 modal_aero_wateruptake_stubs.F90
                                 mpishorthand.F
                                 namelist_utils_stubs.F90
                                 perf_mod_stubs.F90
                                 phys_control_stubs.F90
                                 phys_prop.F90
                                 physconst_stubs.F90
                                 physics_buffer_stubs.F90
                                 physics_types_stubs.F90
                                 ppgrid_stubs.F90
                                 rad_constituents.F90
                                 radconstants_stubs.F90
                                 ref_pres_stubs.F90
                                 scamMod_stubs.F90
                                 shr_assert_mod_stubs.F90
                                 shr_file_mod.F90
                                 shr_infnan_mod_stubs.F90
                                 shr_kind_mod.F90
                                 shr_log_mod_stubs.F90
                                 shr_mpi_mod_stubs.F90
                                 shr_nl_mod.F90
                                 shr_pio_mod.F90
                                 shr_scam_mod_stubs.F90
                                 shr_strconvert_mod.F90
                                 shr_string_mod.F90
                                 shr_sys_mod_stubs.F90
                                 shr_timer_mod_stubs.F90
                                 spmd_utils_stubs.F90
                                 tropopause_stubs.F90
                                 units.F90
                                 wv_saturation_stubs.F90
              )

target_link_libraries(test_current_cam mamlib ${PIO_LIBS})

add_test(NAME current_cam COMMAND run_current_cam.sh)

################################################################################
