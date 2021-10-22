! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_mode module

!> Test program for the mode_t type and related functions
program test_mode

  implicit none

  call test_mode_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_mode_t( )

    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr
    use ai_wavelength_grid,            only : wavelength_grid_t
    use mam_constants,                 only : kAccellerationByGravity
    use mam_mode,                      only : mode_t, mode_state_t
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, die, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_property,               only : property_t

    character(len=*), parameter :: my_name = "mode_t tests"
    type(mode_t) :: mode
    class(aerosol_state_t), pointer :: state_a, state_b
    type(config_t) :: mode_config, optics_config
    class(optics_t), pointer :: optics
    type(optics_ptr), allocatable :: optics_set(:)
    type(environmental_state_t) :: env_state
    type(wavelength_grid_t) :: grid
    type(property_t) :: prop
    type(optics_lookup_t) :: mock_optics_lookup
    real(kind=dk), allocatable :: raw_state_a(:), raw_state_b(:)
    real(kind=dk), allocatable :: optics_values(:)
    real(kind=dk) :: temp_value
    complex(kind=dk), allocatable :: complex_array_a(:), complex_array_b(:)
    real(kind=dk),    allocatable :: absorption_short(:), absorption_long(:)
    real(kind=dk),    allocatable :: extinction_short(:), extinction_long(:)
    real(kind=dk),    allocatable :: asymmetry_short(:), asymmetry_long(:)
    real(kind=dk),    allocatable :: ssa_short(:), aod_short(:), aod_long(:)
    real(kind=dk),    allocatable :: compare_values(:)
    integer :: i_elem, i_band
    logical :: flag

    call mode_config%from_file( "mode_config.json" )
    mode = mode_t( mode_config )

    state_a => mode%new_state( )
    state_b => mode%new_state( )

    ! test mode_state_t functions
    call state_a%randomize( )
    call state_b%randomize( )
    call assert( 119422573, state_a%raw_size( ) .eq. state_b%raw_size( ) )
    allocate( raw_state_a( state_a%raw_size( ) ) )
    allocate( raw_state_b( state_b%raw_size( ) ) )
    call state_a%dump_state( raw_state_a )
    call state_b%dump_state( raw_state_b )
    flag = .true.
    do i_elem = 1, size( raw_state_a )
      flag = flag .and. raw_state_a( i_elem ) .eq. raw_state_b( i_elem )
    end do
    call assert( 904197749, .not. flag )
    call state_b%load_state( raw_state_a )
    call state_b%dump_state( raw_state_b )
    flag = .true.
    do i_elem = 1, size( raw_state_a )
      flag = flag .and. raw_state_a( i_elem ) .eq. raw_state_b( i_elem )
    end do
    call assert( 167637108, flag )

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
    call assert( 800815204, i_elem .eq. 11 + state_a%raw_size( ) )
    i_elem = 16
    call state_b%dump_state( raw_state_b, i_elem )
    call assert( 850030683, i_elem .eq. 16 + state_b%raw_size( ) )
    flag = .true.
    do i_elem = 1, state_a%raw_size( )
      flag = flag .and. raw_state_a( 10 + i_elem ) .eq. raw_state_b( 15 + i_elem )
    end do
    call assert( 173214135, .not. flag )
    i_elem = 11
    call state_b%load_state( raw_state_a, i_elem )
    call assert( 161232282, i_elem .eq. 11 + state_b%raw_size( ) )
    i_elem = 16
    call state_a%dump_state( raw_state_b, i_elem )
    call assert( 157873509, i_elem .eq. 16 + state_b%raw_size( ) )
    flag = .true.
    do i_elem = 1, state_a%raw_size( )
      flag = flag .and. raw_state_a( 10 + i_elem ) .eq. raw_state_b( 15 + i_elem )
    end do
    call assert( 272097388, flag )
    do i_elem = 1, 10
      call assert( 661626675, raw_state_a( i_elem ) .eq. 12.5_dk )
    end do
    do i_elem = 11 + state_a%raw_size( ), 15 + state_a%raw_size( )
      call assert( 642927276, raw_state_a( i_elem ) .eq. 12.5_dk )
    end do
    do i_elem = 1, 15
      call assert( 472770372, raw_state_b( i_elem ) .eq. 43.1_dk )
    end do
    do i_elem = 16 + state_a%raw_size( ), 25 + state_a%raw_size( )
      call assert( 867563966, raw_state_b( i_elem ) .eq. 43.1_dk )
    end do

    deallocate( raw_state_a )
    deallocate( raw_state_b )
    deallocate(     state_a )
    deallocate(     state_b )

    ! grid accessors
    grid = wavelength_grid_t( (/ 12.3_dk, 100.0_dk /),                        &
                              (/ 92.3_dk, 1145.0_dk /) )
    call assert( 588709033, grid .eq. mode%shortwave_grid( ) )
    grid = wavelength_grid_t( (/ 12.3_dk, 200.0_dk /),                        &
                              (/ 92.3_dk, 2290.0_dk /) )
    call assert( 179027108, grid .eq. mode%longwave_grid( ) )

    ! simple accessors/calculators
    state_a => mode%new_state( )
    call state_a%randomize( )
    allocate( raw_state_a( state_a%raw_size( ) ) )
    call state_a%dump_state( raw_state_a )
    ! IMPORTANT: this is just for testing - never rely on the raw state data
    !            outside of tests. It will change often
    associate( wet_diameter => raw_state_a( 1 ),                              &
               number_mr    => raw_state_a( 2 ),                              &
               foo_mass     => raw_state_a( 3 ),                              &
               bar_mass     => raw_state_a( 4 ) )
    select type( state_a )
    type is( mode_state_t )
    call assert( 680874740,                                                   &
          mode%geometric_mean_diameter_of_number_distribution__m( state_a )   &
          .eq. 42.53_dk )
    call assert( 961896750,                                                   &
          mode%geometric_standard_deviation_of_number_distribution( state_a ) &
          .eq. 2.4_dk )
    temp_value = foo_mass * 6.0_dk + bar_mass * 3.0_dk
    call assert( 267834042,                                                   &
                 mode%wet_volume_to_mass_mixing_ratio__m3_kg( state_a )       &
                 .eq. temp_value )
    call assert( 315596282, mode%wet_number_mode_radius__m( state_a )         &
                            .eq. wet_diameter * 0.5_dk )
    call assert( 371981602, mode%wet_number_mode_diameter__m( state_a )       &
                            .eq. wet_diameter )
    temp_value = wet_diameter * exp( 2.0_dk * ( log( 2.4_dk ) )**2 )
    call assert( 102514747, mode%wet_surface_mode_diameter__m( state_a )      &
                            .eq. temp_value )
    temp_value = temp_value * 0.5_dk
    call assert( 543165047, mode%wet_surface_mode_radius__m( state_a )        &
                            .eq. temp_value )
    call assert( 769707271, mode%number_mixing_ratio__num_mol( state_a )      &
                            .eq. number_mr )

    ! net refractive indices
    allocate( complex_array_a( 2 ) )
    allocate( complex_array_b( 2 ) )
    complex_array_a = mode%net_shortwave_refractive_index( state_a, 2 )
    complex_array_b = net_refractive_index( state_a, 2 )
    call assert( 847572900, complex_array_a(1) .eq. complex_array_b(1) )
    call assert( 934499897, complex_array_a(2) .eq. complex_array_b(2) )
    complex_array_a = mode%net_longwave_refractive_index( state_a, 2 )
    complex_array_b = net_refractive_index( state_a, 1 )
    call assert( 764342993, complex_array_a(1) .eq. complex_array_b(1) )
    call assert( 594186089, complex_array_a(2) .eq. complex_array_b(2) )
    deallocate( complex_array_a )
    deallocate( complex_array_b )

    ! optics functions
    call env_state%randomize( )
    allocate( absorption_long(  2 ) )
    allocate( absorption_short( 2 ) )
    allocate( extinction_long(  2 ) )
    allocate( extinction_short( 2 ) )
    allocate( asymmetry_long(   2 ) )
    allocate( asymmetry_short(  2 ) )
    allocate( ssa_short(        2 ) )
    allocate( aod_short(        2 ) )
    allocate( aod_long(         2 ) )
    allocate( compare_values(   2 ) )
    absorption_long  = specific_absorption( mode, state_a, 1 )
    absorption_short = specific_absorption( mode, state_a, 2 )
    extinction_long  = specific_extinction( mode, state_a, 1 )
    extinction_short = specific_extinction( mode, state_a, 2 )
    asymmetry_long   = asymmetry_factor(    mode, state_a, 1 )
    asymmetry_short  = asymmetry_factor(    mode, state_a, 2 )

    absorption_short(:) = min( absorption_short(:), extinction_short(:) )
    ssa_short(:) = 1.0_dk - absorption_short(:) /                             &
                            ( max( extinction_short(:), 1.0e-40_dk ) )
    aod_short(:) = extinction_short(:) * env_state%layer_thickness__Pa( ) /   &
                   kAccellerationByGravity
    aod_long(:)  = absorption_long(:)  * env_state%layer_thickness__Pa( ) /   &
                   kAccellerationByGravity

    ! get optics shortwave
    call optics_config%empty( )
    call optics_config%add( "type", 1, my_name )
    mock_optics_lookup = optics_lookup_t( optics_config )
    grid    =  mode%shortwave_grid( )
    allocate( optics_values( grid%number_of_sections( ) ) )

    ! scalar get optics
    optics  => mode%new_optics( property_t( my_name, "asymmetry factor" ),    &
                                grid )
    call assert( 771601198, optics%native_grid( ) .eq.                        &
                            mock_optics_lookup%grid( ) )
    call assert( 560399600, optics%output_grid( ) .eq. grid )
    call mode%shortwave_optics( env_state, state_a, optics )
    call optics%get_values( optics_values )
    compare_values(:) = aod_short(:) * ssa_short(:) * asymmetry_short(:)
    call assert( 187282691, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 803354202, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 175300838, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 229780624, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    deallocate(        optics )

    ! array get optics
    allocate( optics_set( 3 ) )
    prop = property_t( my_name, "layer extinction optical depth" )
    optics_set( 1 )%ptr_ => mode%new_optics( prop, grid )
    prop = property_t( my_name, "layer single-scatter albedo" )
    optics_set( 2 )%ptr_ => mode%new_optics( prop, grid )
    prop = property_t( my_name, "forward scattered fraction" )
    optics_set( 3 )%ptr_ => mode%new_optics( prop, grid )
    call mode%shortwave_optics( env_state, state_a, optics_set )
    call optics_set( 1 )%ptr_%get_values( optics_values )
    compare_values(:) = aod_short(:)
    do i_band = 1, size( optics_values )
      call assert( 986456716, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 198775062, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 646142908, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    call optics_set( 2 )%ptr_%get_values( optics_values )
    compare_values(:) = aod_short(:) * ssa_short(:)
    do i_band = 1, size( optics_values )
      call assert( 312998941, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 360308886, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 255160382, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    call optics_set( 3 )%ptr_%get_values( optics_values )
    compare_values(:) = aod_short(:) * ssa_short(:) * asymmetry_short(:)      &
                        * asymmetry_short(:)
    do i_band = 1, size( optics_values )
      call assert( 414788672, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 244631768, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 139483264, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do
    deallocate(    optics_set )
    deallocate( optics_values )

    ! get optics longwave
    call optics_config%empty( )
    call optics_config%add( "type", 2, my_name )
    mock_optics_lookup = optics_lookup_t( optics_config )
    grid    =  mode%longwave_grid( )
    allocate( optics_values( grid%number_of_sections( ) ) )

    ! scalar get optics
    prop = property_t( my_name, "layer absorption optical depth" )
    optics  => mode%new_optics( prop, grid )
    call assert( 691311390, optics%native_grid( ) .eq.                        &
                            mock_optics_lookup%grid( ) )
    call assert( 121096585, optics%output_grid( ) .eq. grid )
    call mode%longwave_optics( env_state, state_a, optics )
    call optics%get_values( optics_values )
    compare_values(:) = aod_long(:)
    call assert( 517795713, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 295064557, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 742432403, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 572275499, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do

    ! array get optics
    allocate( optics_set( 1 ) )
    optics_set( 1 )%ptr_ => optics
    call mode%longwave_optics( env_state, state_a, optics_set )
    call optics_set( 1 )%ptr_%get_values( optics_values )
    compare_values(:) = aod_long(:)
    call assert( 836051349, size( optics_values ) .eq. size( compare_values ) )
    do i_band = 1, size( optics_values )
      call assert( 383419196, optics_values( i_band ) .gt. 0.0_dk )
      call assert( 213262292, optics_values( i_band ) .lt. 1.0e200_dk )
      call assert( 108113788, almost_equal( optics_values(  i_band ),         &
                                            compare_values( i_band ) ) )
    end do

    deallocate(     optics )
    deallocate( optics_set )


    class default
      call die( 889482580 )
    end select
    end associate
    deallocate( state_a )

  end subroutine test_mode_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function net_refractive_index( mode_state, long_or_short )

    use mam_mode,                      only : mode_state_t
    use musica_assert,                 only : die
    use musica_constants,              only : dk => musica_dk

    complex(kind=dk)                :: net_refractive_index(2)
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: long_or_short ! 1=long; 2=short

    real(kind=dk), allocatable :: raw_state(:)
    real(kind=dk) :: real_comp, imag_comp

    ! select coefficients for long/short wave refractive index in mock_species
    select case( long_or_short )
    case( 1 )
      real_comp = 15.6_dk
      imag_comp = 1.32_dk
    case( 2 )
      real_comp = 12.5_dk
      imag_comp = 2.53_dk
    case default
      call die( 530770503 )
    end select

    allocate( raw_state( mode_state%raw_size( ) ) )
    call mode_state%dump_state( raw_state )
    ! IMPORTANT: this is just for testing - never rely on the raw state data
    !            outside of tests. It will change often
    associate( foo_mmr => raw_state(3), bar_mmr => raw_state(4) )
      net_refractive_index(1) =                                               &
          cmplx( real_comp, 6 + imag_comp, kind=dk ) * ( foo_mmr * 6 ) +      &
          cmplx( real_comp, 3 + imag_comp, kind=dk ) * ( bar_mmr * 3 )
      net_refractive_index(2) =                                               &
          cmplx( 2 * real_comp, 6 + imag_comp, kind=dk ) * ( foo_mmr * 6 ) +  &
          cmplx( 2 * real_comp, 3 + imag_comp, kind=dk ) * ( bar_mmr * 3 )
      net_refractive_index(:) = net_refractive_index(:)                       &
                                / max( foo_mmr * 6 + bar_mmr * 3, 1.0e-60_dk )
    end associate

  end function net_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function specific_absorption( mode, mode_state, long_or_short )

    use mam_constants,                 only : kWaterDensitySTP
    use mam_mode,                      only : mode_t, mode_state_t
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, die, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_math,                   only : chebyshev_function,             &
                                              weighted_chebyshev

    real(kind=dk)                   :: specific_absorption(2)
    class(mode_t),       intent(in) :: mode
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: long_or_short ! 1=long, 2=short

    character(len=*), parameter :: my_name = "mode absorption test"
    type(config_t)              :: lookup_config
    type(optics_lookup_t)       :: mock_optics_lookup
    real(kind=dk)               :: cheby_coeff(3,2)
    real(kind=dk)               :: cheby_func(3)
    real(kind=dk)               :: normal_radius, wet_surface_radius
    real(kind=dk)               :: compare_values(2)
    real(kind=dk), allocatable  :: raw_state(:)

    call lookup_config%empty( )
    select case( long_or_short )
    case( 1 )
      call lookup_config%add( "type", 2, my_name )
    case( 2 )
      call lookup_config%add( "type", 1, my_name )
    case default
      call die( 373295283 )
    end select
    mock_optics_lookup = optics_lookup_t( lookup_config )
    call mock_optics_lookup%get_optics( (/ ( 0.0_dk, 0.0_dk ),                &
                                           ( 0.0_dk, 0.0_dk ) /),             &
                                        absorption = cheby_coeff )
    allocate( raw_state( mode_state%raw_size( ) ) )
    call mode_state%dump_state( raw_state )
    ! IMPORTANT: this is just for testing - never rely on the raw state data
    !            outside of tests. It will change often
    associate( wet_diameter => raw_state(1),                                  &
               foo_mass     => raw_state(3),                                  &
               bar_mass     => raw_state(4) )
    wet_surface_radius = ( 0.5_dk * wet_diameter )                            &
                         * exp( 2.0_dk * ( log( 2.4_dk ) )**2 )
    normal_radius = mock_optics_lookup%normalize_radius( wet_surface_radius )
    call chebyshev_function( normal_radius, cheby_func )
    specific_absorption(1) =                                                  &
        weighted_chebyshev( 3, cheby_coeff(:,1), cheby_func )                 &
        ! wet volume to mass mixing ratio
        * ( foo_mass * 6 + bar_mass * 3 ) * kWaterDensitySTP
    specific_absorption(1) = max( specific_absorption(1), 0.0_dk )
    specific_absorption(2) =                                                  &
        weighted_chebyshev( 3, cheby_coeff(:,2), cheby_func )                 &
        ! wet volume to mass mixing ratio
        * ( foo_mass * 6 + bar_mass * 3 ) * kWaterDensitySTP
    specific_absorption(2) = max( specific_absorption(2), 0.0_dk )
    end associate

    compare_values = mode%specific_absorption__m2_kg( mode_state, 2, 3,       &
                                                      cheby_coeff, cheby_func )
    call assert( 715576209, compare_values(1) .gt. 0.0_dk )
    call assert( 770055995, compare_values(1) .lt. 1e200_dk )
    call assert( 446787188,                                                   &
                 almost_equal( compare_values(1), specific_absorption(1) ) )
    call assert( 654378877, compare_values(2) .gt. 0.0_dk )
    call assert( 201746724, compare_values(2) .lt. 1e200_dk )
    call assert( 501266974,                                                   &
                 almost_equal( compare_values(2), specific_absorption(2) ) )
    compare_values =                                                          &
        mode%specific_absorption__m2_kg( mode_state, 2, 3,cheby_coeff,        &
                                       cheby_func,                            &
                                       max_absorption = (/ 0.0_dk, 0.0_dk /) )
    call assert( 656083252, compare_values(1) .eq. 0.0_dk )
    call assert( 485926348, compare_values(2) .eq. 0.0_dk )

  end function specific_absorption

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function specific_extinction( mode, mode_state, long_or_short )

    use mam_constants,                 only : kWaterDensitySTP
    use mam_mode,                      only : mode_t, mode_state_t
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, die, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_math,                   only : chebyshev_function,             &
                                              weighted_chebyshev

    real(kind=dk)                   :: specific_extinction(2)
    class(mode_t),       intent(in) :: mode
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: long_or_short ! 1=long, 2=short

    character(len=*), parameter :: my_name = "mode extinction test"
    type(config_t)              :: lookup_config
    type(optics_lookup_t)       :: mock_optics_lookup
    real(kind=dk)               :: cheby_coeff(3,2)
    real(kind=dk)               :: cheby_func(3)
    real(kind=dk)               :: normal_radius, wet_surface_radius
    real(kind=dk)               :: compare_values(2)
    real(kind=dk), allocatable  :: raw_state(:)

    call lookup_config%empty( )
    select case( long_or_short )
    case( 1 )
      call lookup_config%add( "type", 2, my_name )
    case( 2 )
      call lookup_config%add( "type", 1, my_name )
    case default
      call die( 995833204 )
    end select
    mock_optics_lookup = optics_lookup_t( lookup_config )
    call mock_optics_lookup%get_optics( (/ ( 0.0_dk, 0.0_dk ),                &
                                           ( 0.0_dk, 0.0_dk ) /),             &
                                        extinction = cheby_coeff )
    allocate( raw_state( mode_state%raw_size( ) ) )
    call mode_state%dump_state( raw_state )
    ! IMPORTANT: this is just for testing - never rely on the raw state data
    !            outside of tests. It will change often
    associate( wet_diameter => raw_state(1),                                  &
               foo_mass     => raw_state(3),                                  &
               bar_mass     => raw_state(4) )
    wet_surface_radius = ( 0.5_dk * wet_diameter )                            &
                         * exp( 2.0_dk * ( log( 2.4_dk ) )**2 )
    normal_radius = mock_optics_lookup%normalize_radius( wet_surface_radius )
    call chebyshev_function( normal_radius, cheby_func )
    if( wet_surface_radius .le. mock_optics_lookup%maximum_radius__m( ) ) then
      specific_extinction(1) =                                                &
          exp( weighted_chebyshev( 3, cheby_coeff(:,1), cheby_func ) )
      specific_extinction(2) =                                                &
          exp( weighted_chebyshev( 3, cheby_coeff(:,2), cheby_func ) )
    else
      specific_extinction(:) =                                                &
          1.5_dk / ( wet_surface_radius * kWaterDensitySTP )
    end if
    specific_extinction(:) = specific_extinction(:)                           &
        ! wet volume to mass mixing ratio
        * ( foo_mass * 6 + bar_mass * 3 ) * kWaterDensitySTP
    end associate

    compare_values = mode%specific_extinction__m2_kg( mode_state, 2, 3,       &
                                                      cheby_coeff, cheby_func,&
                                                      mock_optics_lookup )
    call assert( 933495353, compare_values(1) .gt. 0.0_dk )
    call assert( 480863200, compare_values(1) .lt. 1e200_dk )
    call assert( 108102181,                                                   &
                 almost_equal( compare_values(1), specific_extinction(1) ) )
    call assert( 310706296, compare_values(2) .gt. 0.0_dk )
    call assert( 140549392, compare_values(2) .lt. 1e200_dk )
    call assert( 837945276,                                                   &
                 almost_equal( compare_values(2), specific_extinction(2) ) )

  end function specific_extinction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function asymmetry_factor( mode, mode_state, long_or_short )

    use mam_constants,                 only : kWaterDensitySTP
    use mam_mode,                      only : mode_t, mode_state_t
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, die, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_math,                   only : chebyshev_function,             &
                                              weighted_chebyshev

    real(kind=dk)                   :: asymmetry_factor(2)
    class(mode_t),       intent(in) :: mode
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: long_or_short ! 1=long, 2=short

    character(len=*), parameter :: my_name = "mode asymmetry factor test"
    type(config_t)              :: lookup_config
    type(optics_lookup_t)       :: mock_optics_lookup
    real(kind=dk)               :: cheby_coeff(3,2)
    real(kind=dk)               :: cheby_func(3)
    real(kind=dk)               :: normal_radius, wet_surface_radius
    real(kind=dk)               :: compare_values(2)
    real(kind=dk), allocatable  :: raw_state(:)

    call lookup_config%empty( )
    select case( long_or_short )
    case( 1 )
      call lookup_config%add( "type", 2, my_name )
    case( 2 )
      call lookup_config%add( "type", 1, my_name )
    case default
      call die( 567537861 )
    end select
    mock_optics_lookup = optics_lookup_t( lookup_config )
    call mock_optics_lookup%get_optics( (/ ( 0.0_dk, 0.0_dk ),                &
                                           ( 0.0_dk, 0.0_dk ) /),             &
                                        asymmetry_factor = cheby_coeff )
    allocate( raw_state( mode_state%raw_size( ) ) )
    call mode_state%dump_state( raw_state )
    ! IMPORTANT: this is just for testing - never rely on the raw state data
    !            outside of tests. It will change often
    associate( wet_diameter => raw_state(1) )
    wet_surface_radius = ( 0.5_dk * wet_diameter )                            &
                         * exp( 2.0_dk * ( log( 2.4_dk ) )**2 )
    normal_radius = mock_optics_lookup%normalize_radius( wet_surface_radius )
    call chebyshev_function( normal_radius, cheby_func )
    asymmetry_factor(1) = weighted_chebyshev( 3, cheby_coeff(:,1), cheby_func )
    asymmetry_factor(2) = weighted_chebyshev( 3, cheby_coeff(:,2), cheby_func )
    end associate

    compare_values = mode%asymmetry_factor( mode_state, 2, 3, cheby_coeff,    &
                                            cheby_func )
    call assert( 174649801, compare_values(1) .gt. 0.0_dk )
    call assert( 904492896, compare_values(1) .lt. 1e200_dk )
    call assert( 734335992,                                                   &
                 almost_equal( compare_values(1), asymmetry_factor(1) ) )
    call assert( 281703839, compare_values(2) .gt. 0.0_dk )
    call assert( 394022184, compare_values(2) .lt. 1e200_dk )
    call assert( 223865280,                                                   &
                 almost_equal( compare_values(2), asymmetry_factor(2) ) )

  end function asymmetry_factor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_mode
