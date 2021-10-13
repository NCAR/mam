! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_optics_util module

!> Tests for optics utility functions
program test_optics_util

  implicit none

  call test_optics_util_functions( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_optics_util_functions( )

    use ai_wavelength_grid,            only : wavelength_grid_t
    use ai_optics,                     only : optics_t
    use mam_optics_util,               only : create_optics,                  &
                                              add_shortwave_property,         &
                                              add_longwave_property
    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_property,               only : property_t
    use mock_interpolation_strategy,   only : foo_mapper

    character(len=*), parameter :: my_name = "optics utility tests"
    type(wavelength_grid_t)     :: native_sw_grid, native_lw_grid, output_grid
    type(property_t)            :: property
    class(optics_t), pointer    :: optics
    real(kind=dk)               :: values(3)

    native_sw_grid = wavelength_grid_t( (/  0.0_dk,  10.0_dk, 200.0_dk /),    &
                                        (/ 10.0_dk, 110.0_dk, 400.0_dk /) )
    native_lw_grid = wavelength_grid_t( (/ 10.0_dk, 400.0_dk /),              &
                                        (/ 20.0_dk, 600.0_dk /) )

    ! no interpolation
    output_grid    =  native_sw_grid
    property       =  property_t( my_name,                                    &
                                  name = "layer extinction optical depth" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 0.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 201129633, values(1) .eq. 150.0_dk )
    call assert( 930972728, values(2) .eq.   0.0_dk )
    call assert( 143291074, values(3) .eq.  10.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "layer single-scatter albedo" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 0.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 692900946, values(1) .eq. 300.0_dk )
    call assert( 805219291, values(2) .eq.   0.0_dk )
    call assert( 635062387, values(3) .eq.  10.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "asymmetry factor" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 0.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 914178863, values(1) .eq. 3000.0_dk )
    call assert( 461546710, values(2) .eq.    0.0_dk )
    call assert( 356398206, values(3) .eq.  250.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "forward scattered fraction" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 0.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 630250375, values(1) .eq. 30000.0_dk )
    call assert( 177618222, values(2) .eq.     0.0_dk )
    call assert( 907461317, values(3) .eq.  6250.0_dk )
    deallocate( optics )

    output_grid    =  native_lw_grid
    property       =  property_t( my_name,                                    &
                                  name = "layer absorption optical depth" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid )
    call optics%set_values( (/ 0.0_dk, 0.0_dk /) )
    call add_longwave_property( (/ 150.0_dk, 0.0_dk /), optics )
    values(:) = 0.0_dk
    call optics%get_values( values(1:2) )
    call assert( 228739235, values(1) .eq. 150.0_dk )
    call assert( 676107081, values(2) .eq.   0.0_dk )
    call assert( 223474928, values(3) .eq.   0.0_dk )
    deallocate( optics )

    ! custom interpolation strategy
    output_grid    = wavelength_grid_t( (/  4.0_dk,  14.0_dk, 210.0_dk /),    &
                                        (/ 13.0_dk, 105.0_dk, 403.0_dk /) )
    property       =  property_t( my_name,                                    &
                                  name = "layer extinction optical depth" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid,                             &
                                     interpolation_strategy = foo_mapper )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 1.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 577249425, values(1) .eq. 150.5_dk )
    call assert( 689567770, values(2) .eq.   0.5_dk )
    call assert( 519410866, values(3) .eq.   0.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "layer single-scatter albedo" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid,                             &
                                     interpolation_strategy = foo_mapper )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 1.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 110016064, values(1) .eq. 302.5_dk )
    call assert( 287342809, values(2) .eq.   2.5_dk )
    call assert( 734710655, values(3) .eq.   0.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "asymmetry factor" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid,                             &
                                     interpolation_strategy = foo_mapper )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 1.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 283984036, values(1) .eq. 3010.0_dk )
    call assert( 113827132, values(2) .eq.   10.0_dk )
    call assert( 561194978, values(3) .eq.    0.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "forward scattered fraction" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid,                             &
                                     interpolation_strategy = foo_mapper )
    call optics%set_values( (/ 0.0_dk, 0.0_dk, 0.0_dk /) )
    call add_shortwave_property( (/ 150.0_dk, 1.0_dk, 10.0_dk /),             &
                                 (/   2.0_dk, 5.0_dk,  1.0_dk /),             &
                                 (/  10.0_dk, 4.0_dk, 25.0_dk /),             &
                                 optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 892885706, values(1) .eq. 30040.0_dk )
    call assert( 440253553, values(2) .eq.    40.0_dk )
    call assert( 335105049, values(3) .eq.     0.0_dk )
    deallocate( optics )

    property       =  property_t( my_name,                                    &
                                  name = "layer absorption optical depth" )
    optics         => create_optics( property, native_sw_grid, native_lw_grid,&
                                     output_grid,                             &
                                     interpolation_strategy = foo_mapper )
    call optics%set_values( (/ 0.0_dk, 0.0_dk /) )
    call add_longwave_property( (/ 150.0_dk, 1.0_dk /), optics )
    values(:) = 0.0_dk
    call optics%get_values( values )
    call assert( 214163624, values(1) .eq. 150.5_dk )
    call assert( 944006719, values(2) .eq.   0.5_dk )
    call assert( 491374566, values(3) .eq.   0.0_dk )
    deallocate( optics )

  end subroutine test_optics_util_functions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_optics_util
