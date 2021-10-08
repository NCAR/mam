! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_optics_lookup module

!> Test module for the mam_optics_lookup module
program test_optics_lookup

  implicit none

  call test_optics_lookup_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_optics_lookup_t( )

    use ai_wavelength_grid,            only : wavelength_grid_t,              &
                                              kWavenumber, kCentimeter
    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk

    integer, parameter :: kNumberOfBands = 3
    integer, parameter :: kNumberOfCoefficients = 2
    type(optics_lookup_t) :: a, b, c
    type(config_t) :: config
    type(wavelength_grid_t) :: compare_grid
    complex(kind=dk) :: ref_ind( kNumberOfBands )
    real(kind=dk) :: absorp( kNumberOfCoefficients, kNumberOfBands )
    real(kind=dk) :: ext(    kNumberOfCoefficients, kNumberOfBands )
    real(kind=dk) :: asym(   kNumberOfCoefficients, kNumberOfBands )

    ! set up grid to compare with loaded values
    compare_grid = wavelength_grid_t( (/ 0.1_dk, 2.3_dk,  4.3_dk /),          &
                                      (/ 0.9_dk, 4.3_dk, 10.2_dk /),          &
                                      bounds_in = kWavenumber,                &
                                      base_unit = kCentimeter )

    !! lookup table w/o any optical properties

    call config%from_file( "optics_lookup_a_config.json" )
    a = optics_lookup_t( config )

    ! check wavelength grid
    call assert( 903443825, a%grid( ) .eq. compare_grid )

    ! check number of wavelength bands
    call assert( 173056488, a%number_of_wavelength_bands( )                   &
                            .eq. kNumberOfBands )

    ! check number of Chebyshev coefficients
    call assert( 218913194, a%number_of_chebyshev_coefficients( )             &
                            .eq. 0 )

    ! check maximum radius
    call assert( 101473010,                                                   &
                 almost_equal( a%maximum_radius__m( ), 1.0e-06_dk ) )

    ! check minimum radius
    call assert( 435069272,                                                   &
                 almost_equal( a%minimum_radius__m( ), 1.0e-08_dk ) )

    ! normalized radius
    call assert( 886248186, a%normalize_radius( 1.0e-09_dk )           &
                            .eq. -1.0_dk )
    call assert( 535858059,                                                   &
                 almost_equal( a%normalize_radius( 1.0e-07_dk ),       &
                               0.0_dk ) )
    call assert( 983225905, a%normalize_radius( 1.0e-05_dk )           &
                            .eq. 1.0_dk )

    ! get optics (returns nothing)
    ref_ind(:) = cmplx( 0.0_dk, 0.0_dk )
    call a%get_optics( ref_ind )


    !! lookup table w/ absorption

    call config%from_file( "optics_lookup_b_config.json" )
    b = optics_lookup_t( config )

    ! check wavelength grid
    call assert( 842246493, b%grid( ) .eq. compare_grid )

    ! check number of wavelength bands
    call assert( 499935204, b%number_of_wavelength_bands( )                   &
                            .eq. kNumberOfBands )

    ! check number of Chebyshev coefficients
    call assert( 612253549, b%number_of_chebyshev_coefficients( )             &
                            .eq. kNumberOfCoefficients )

    ! check maximum radius
    call assert( 747335190,                                                   &
                 almost_equal( b%maximum_radius__m( ), 1.0e-06_dk ) )

    ! check minimum radius
    call assert( 177120385,                                                   &
                 almost_equal( b%minimum_radius__m( ), 1.0e-08_dk ) )

    ! normalized radius
    call assert( 854389228, b%normalize_radius( 1.0e-09_dk )           &
                            .eq. -1.0_dk )
    call assert( 684232324,                                                   &
                 almost_equal( b%normalize_radius( 1.0e-07_dk ),       &
                               0.0_dk ) )
    call assert( 796550669, b%normalize_radius( 1.0e-05_dk )           &
                            .eq. 1.0_dk )

    ! get optics (absorption)
    ref_ind(2) = cmplx( 5.0_dk, 60.0_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 543590556, absorp(1,1) .eq. 0.0_dk )
    call assert( 422649131, absorp(2,1) .eq. 0.0_dk )
    call assert( 252492227, absorp(1,2) .eq. 4.0_dk )
    call assert( 764868473, absorp(2,2) .eq. 7.0_dk )
    call assert( 312236320, absorp(1,3) .eq. 0.0_dk )
    call assert( 759604166, absorp(2,3) .eq. 0.0_dk )

    ref_ind(2) = cmplx( 5.0_dk, 50.0_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 709234178, absorp(1,1) .eq. 0.0_dk )
    call assert( 646131312, absorp(2,1) .eq. 0.0_dk )
    call assert( 475974408, absorp(1,2) .eq. 3.0_dk )
    call assert( 588292753, absorp(2,2) .eq. 6.0_dk )
    call assert( 135660600, absorp(1,3) .eq. 0.0_dk )
    call assert( 312987345, absorp(2,3) .eq. 0.0_dk )

    ref_ind(2) = cmplx( 6.0_dk, 60.0_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 299099958, absorp(1,1) .eq.  0.0_dk )
    call assert( 411418303, absorp(2,1) .eq.  0.0_dk )
    call assert( 306269799, absorp(1,2) .eq.  9.0_dk )
    call assert( 753637645, absorp(2,2) .eq. 14.0_dk )
    call assert( 301005492, absorp(1,3) .eq.  0.0_dk )
    call assert( 130848588, absorp(2,3) .eq.  0.0_dk )

    ! interpolation tests
    ref_ind(2) = cmplx( 5.5_dk, 60.0_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 313726763, absorp(1,1) .eq.  0.0_dk )
    call assert( 761094609, absorp(2,1) .eq.  0.0_dk )
    call assert( 873412954, absorp(1,2) .eq.  6.5_dk )
    call assert( 420780801, absorp(2,2) .eq. 10.5_dk )
    call assert( 868148647, absorp(1,3) .eq.  0.0_dk )
    call assert( 697991743, absorp(2,3) .eq.  0.0_dk )

    ref_ind(2) = cmplx( 5.0_dk, 55.0_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 527834839, absorp(1,1) .eq. 0.0_dk )
    call assert( 422686335, absorp(2,1) .eq. 0.0_dk )
    call assert( 252529431, absorp(1,2) .eq. 3.5_dk )
    call assert( 982372526, absorp(2,2) .eq. 6.5_dk )
    call assert( 812215622, absorp(1,3) .eq. 0.0_dk )
    call assert( 359583469, absorp(2,3) .eq. 0.0_dk )

    !! lookup table w/ absorption, extinction, and asymmetry factor

    call config%from_file( "optics_lookup_c_config.json" )
    c = optics_lookup_t( config )

    ! check wavelength grid
    call assert( 561676778, c%grid( ) .eq. compare_grid )

    ! check number of wavelength bands
    call assert( 831625932, c%number_of_wavelength_bands( )                   &
                            .eq. kNumberOfBands )

    ! check number of Chebyshev coefficients
    call assert( 273845275, c%number_of_chebyshev_coefficients( )             &
                            .eq. kNumberOfCoefficients )

    ! check maximum radius
    call assert( 967771318,                                                   &
                 almost_equal( c%maximum_radius__m( ), 1.0e-06_dk ) )

    ! check minimum radius
    call assert( 127515412,                                                   &
                 almost_equal( c%minimum_radius__m( ), 1.0e-08_dk ) )

    ! normalized radius
    call assert( 122251105, c%normalize_radius( 1.0e-09_dk )           &
                            .eq. -1.0_dk )
    call assert( 517044699,                                                   &
                 almost_equal( c%normalize_radius( 1.0e-07_dk ),       &
                               0.0_dk ) )
    call assert( 511780392, c%normalize_radius( 1.0e-05_dk )           &
                            .eq. 1.0_dk )

    ! get optics (absorption)
    ref_ind(2) = cmplx( 5.0_dk, 60.0_dk )
    call c%get_optics( ref_ind, absorption = absorp, extinction = ext,        &
                       asymmetry_factor = asym )

    call assert( 622645498, absorp(1,1) .eq. 0.0_dk )
    call assert( 170013345, absorp(2,1) .eq. 0.0_dk )
    call assert( 899856440, absorp(1,2) .eq. 4.0_dk )
    call assert( 447224287, absorp(2,2) .eq. 7.0_dk )
    call assert( 342075783, absorp(1,3) .eq. 0.0_dk )
    call assert( 171918879, absorp(2,3) .eq. 0.0_dk )

    call assert( 724887524, ext(1,1) .eq. 0.0_dk )
    call assert( 272255371, ext(2,1) .eq. 0.0_dk )
    call assert( 719623217, ext(1,2) .eq. 7.0_dk )
    call assert( 266991064, ext(2,2) .eq. 2.0_dk )
    call assert( 161842560, ext(1,3) .eq. 0.0_dk )
    call assert( 609210406, ext(2,3) .eq. 0.0_dk )

    call assert( 721528751, asym(1,1) .eq.  0.0_dk )
    call assert( 551371847, asym(2,1) .eq.  0.0_dk )
    call assert( 998739693, asym(1,2) .eq.  9.0_dk )
    call assert( 546107540, asym(2,2) .eq. 11.0_dk )
    call assert( 440959036, asym(1,3) .eq.  0.0_dk )
    call assert( 605851633, asym(2,3) .eq.  0.0_dk )

  end subroutine test_optics_lookup_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_optics_lookup
