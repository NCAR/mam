################################################################################
# Modal aerosol model

set( MAM_SRC constants.F90
             core.F90
             mode.F90
             optics_lookup.F90
             optics_util.F90
             species.F90
   )

add_library(mamlib ${MAM_SRC})

target_link_libraries(mamlib aerosollib musicalib ${NETCDF_LIBS})

set_target_properties(mamlib PROPERTIES OUTPUT_NAME mam)

################################################################################
