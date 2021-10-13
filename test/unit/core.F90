! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_core module

!> Test program for the core_t type and related functions
program test_core

  implicit none

  call test_core_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_core_t( )

    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr
    use ai_wavelength_grid,            only : wavelength_grid_t
    use mam_core,                      only : core_t, state_t
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_property,               only : property_t

    character(len=*), parameter :: my_name = "core_t tests"
    type(core_t), pointer :: core
    class(aerosol_state_t), pointer :: state_a, state_b
    type(config_t) :: core_config, optics_config
    class(optics_t), pointer :: optics
    type(optics_ptr), allocatable :: optics_set(:)
    type(environmental_state_t) :: env_state
    type(wavelength_grid_t) :: grid
    type(property_t) :: prop
    type(optics_lookup_t) :: mock_optics_lookup
    real(kind=dk), allocatable :: raw_state_a(:), raw_state_b(:)
    real(kind=dk), allocatable :: optics_values(:)
    real(kind=dk), allocatable :: compare_values(:)
    integer :: i_elem, i_band
    logical :: flag

    call core_config%from_file( "core_config.json" )
    core => core_t( core_config )

    state_a => core%new_state( )
    state_b => core%new_state( )

    ! test state_t functions
    call state_a%randomize( )
    call state_b%randomize( )
    call assert( 240883501, state_a%raw_size( ) .eq. state_b%raw_size( ) )
    allocate( raw_state_a( state_a%raw_size( ) ) )
    allocate( raw_state_b( state_b%raw_size( ) ) )
    call state_a%dump_state( raw_state_a )
    call state_b%dump_state( raw_state_b )
    flag = .true.
    do i_elem = 1, size( raw_state_a )
      flag = flag .and. raw_state_a( i_elem ) .eq. raw_state_b( i_elem )
    end do
    call assert( 635677095, .not. flag )
    call state_b%load_state( raw_state_a )
    call state_b%dump_state( raw_state_b )
    flag = .true.
    do i_elem = 1, size( raw_state_a )
      flag = flag .and. raw_state_a( i_elem ) .eq. raw_state_b( i_elem )
    end do
    call assert( 747995440, flag )
    deallocate( raw_state_a )
    deallocate( raw_state_b )

    allocate( raw_state_a( state_a%raw_size( ) + 15 ) )
    allocate( raw_state_b( state_b%raw_size( ) + 25 ) )
    raw_state_a(:) = 12.5_dk
    raw_state_b(:) = 43.1_dk
    call state_a%randomize( )
    call state_b%randomize( )
    i_elem = 11
    call state_a%dump_state( raw_state_a, i_elem )
    call assert( 860313785, i_elem .eq. 11 + state_a%raw_size( ) )
    i_elem = 16
    call state_b%dump_state( raw_state_b, i_elem )
    call assert( 690156881, i_elem .eq. 16 + state_b%raw_size( ) )
    flag = .true.
    do i_elem = 1, state_a%raw_size( )
      flag = flag .and. raw_state_a( 10 + i_elem ) .eq. raw_state_b( 15 + i_elem )
    end do
    call assert( 519999977, .not. flag )
    i_elem = 11
    call state_b%load_state( raw_state_a, i_elem )
    call assert( 632318322, i_elem .eq. 11 + state_b%raw_size( ) )
    i_elem = 16
    call state_a%dump_state( raw_state_b, i_elem )
    call assert( 462161418, i_elem .eq. 16 + state_b%raw_size( ) )
    flag = .true.
    do i_elem = 1, state_a%raw_size( )
      flag = flag .and. raw_state_a( 10 + i_elem ) .eq. raw_state_b( 15 + i_elem )
    end do
    call assert( 856955012, flag )
    do i_elem = 1, 10
      call assert( 673363016, raw_state_a( i_elem ) .eq. 12.5_dk )
    end do
    do i_elem = 11 + state_a%raw_size( ), 15 + state_a%raw_size( )
      call assert( 686798108, raw_state_a( i_elem ) .eq. 12.5_dk )
    end do
    do i_elem = 1, 15
      call assert( 681533801, raw_state_b( i_elem ) .eq. 43.1_dk )
    end do
    do i_elem = 16 + state_a%raw_size( ), 25 + state_a%raw_size( )
      call assert( 511376897, raw_state_b( i_elem ) .eq. 43.1_dk )
    end do

    deallocate( raw_state_a )
    deallocate( raw_state_b )

    allocate( compare_values( 2 ) )
    call env_state%randomize( )

    ! get optics shortwave
    call optics_config%empty( )
    call optics_config%add( "type", 1, my_name )
    mock_optics_lookup = optics_lookup_t( optics_config )
    grid = wavelength_grid_t( (/ 12.3_dk,  100.0_dk /),                       &
                              (/ 92.3_dk, 1145.0_dk /) )
    allocate( optics_values( grid%number_of_sections( ) ) )

    ! scalar optics
    prop = property_t( my_name, "asymmetry factor" )
    optics => core%new_optics( prop, grid )
    call assert( 842692121, optics%output_grid( ) .eq. grid )
    call core%shortwave_optics( env_state, state_a, optics )
    call optics%get_values( optics_values )
    compare_values(:) = (/ 6.0_dk, 5.4_dk /) * 2
    call assert( 267690901, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 941600971, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 488968818, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 601287163, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    deallocate(        optics )

    ! array optics
    allocate( optics_set( 3 ) )
    prop = property_t( my_name, "layer extinction optical depth" )
    optics_set( 1 )%ptr_ => core%new_optics( prop, grid )
    prop = property_t( my_name, "layer single-scatter albedo" )
    optics_set( 2 )%ptr_ => core%new_optics( prop, grid )
    prop = property_t( my_name, "forward scattered fraction" )
    optics_set( 3 )%ptr_ => core%new_optics( prop, grid )
    call core%shortwave_optics( env_state, state_a, optics_set )
    call optics_set( 1 )%ptr_%get_values( optics_values )
    compare_values(:) = (/ 2.0_dk, 8.5_dk /) * 2
    do i_band = 1, size( optics_values )
      call assert( 706799581, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 766543674, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 991180364, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    call optics_set( 2 )%ptr_%get_values( optics_values )
    compare_values(:) = (/ 4.0_dk, 1.8_dk /) * 2
    do i_band = 1, size( optics_values )
      call assert( 138490310, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 315817055, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 763184901, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    call optics_set( 3 )%ptr_%get_values( optics_values )
    compare_values(:) = (/ 8.0_dk, 3.2_dk /) * 2
    do i_band = 1, size( optics_values )
      call assert( 257978496, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 987821591, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 817664687, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    deallocate(    optics_set )
    deallocate( optics_values )

    ! get optics longwave
    call optics_config%empty( )
    call optics_config%add( "type", 2, my_name )
    mock_optics_lookup = optics_lookup_t( optics_config )
    grid = wavelength_grid_t( (/ 12.3_dk,  100.0_dk * 2 /),                   &
                              (/ 92.3_dk, 1145.0_dk * 2 /) )
    allocate( optics_values( grid%number_of_sections( ) ) )

    ! scalar get optics
    prop = property_t( my_name, "layer absorption optical depth" )
    optics  => core%new_optics( prop, grid )
    call assert( 516490039, optics%native_grid( ) .eq.                        &
                            mock_optics_lookup%grid( ) )
    call assert( 346333135, optics%output_grid( ) .eq. grid )
    call core%longwave_optics( env_state, state_a, optics )
    call optics%get_values( optics_values )
    compare_values(:) = (/ 20.0_dk, 82.5_dk /) * 2
    call assert( 855350608, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 402718455, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 850086301, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 397454148, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do

    ! array get optics
    allocate( optics_set( 1 ) )
    optics_set( 1 )%ptr_ => optics
    call core%longwave_optics( env_state, state_a, optics_set )
    call optics_set( 1 )%ptr_%get_values( optics_values )
    compare_values(:) = (/ 20.0_dk, 82.5_dk /) * 2
    call assert( 339615589, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 169458685, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 346785430, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 794153276, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do

    deallocate(        optics )
    deallocate(    optics_set )
    deallocate( optics_values )
    deallocate(       state_a )
    deallocate(       state_b )

  end subroutine test_core_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_core
