################################################################################
# Modal aerosol model

set( MAM_SRC constants.F90
             core.F90
             interpolator.F90
             mode.F90
             mode_shortwave_optics_lookup.F90
             mode_longwave_optics_lookup.F90
             optics_accessor.F90
             optics_constants.F90
             species.F90
   )

add_library(mamlib ${MAM_SRC})

target_link_libraries(mamlib aerosollib musicalib ${NETCDF_LIBS})

set_target_properties(mamlib PROPERTIES OUTPUT_NAME mam)

################################################################################
