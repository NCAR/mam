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

    use mam_optics_lookup,             only : optics_lookup_t
    use musica_assert,                 only : assert, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : musica_dk

    integer, parameter :: kNumberOfBands = 3
    integer, parameter :: kNumberOfCoefficients = 2
    type(optics_lookup_t) :: a, b, c
    type(config_t) :: config
    complex(kind=musica_dk) :: ref_ind( kNumberOfBands )
    real(kind=musica_dk) :: absorp( kNumberOfCoefficients, kNumberOfBands )
    real(kind=musica_dk) :: ext(    kNumberOfCoefficients, kNumberOfBands )
    real(kind=musica_dk) :: asym(   kNumberOfCoefficients, kNumberOfBands )

    !! lookup table w/o any optical properties

    call config%from_file( "optics_lookup_a_config.json" )
    a = optics_lookup_t( config )

    ! check maximum radius
    call assert( 101473010,                                                   &
                 almost_equal( a%maximum_radius__m( ), 1.0e-06_musica_dk ) )

    ! check minimum radius
    call assert( 435069272,                                                   &
                 almost_equal( a%minimum_radius__m( ), 1.0e-08_musica_dk ) )

    ! normalized radius
    call assert( 886248186, a%normalize_radius( 1.0e-09_musica_dk )           &
                            .eq. -1.0_musica_dk )
    call assert( 535858059,                                                   &
                 almost_equal( a%normalize_radius( 1.0e-07_musica_dk ),       &
                               0.0_musica_dk ) )
    call assert( 983225905, a%normalize_radius( 1.0e-05_musica_dk )           &
                            .eq. 1.0_musica_dk )

    ! get optics (returns nothing)
    ref_ind(:) = cmplx( 0.0_musica_dk, 0.0_musica_dk )
    call a%get_optics( ref_ind )


    !! lookup table w/ absorption

    call config%from_file( "optics_lookup_b_config.json" )
    b = optics_lookup_t( config )

    ! check maximum radius
    call assert( 747335190,                                                   &
                 almost_equal( b%maximum_radius__m( ), 1.0e-06_musica_dk ) )

    ! check minimum radius
    call assert( 177120385,                                                   &
                 almost_equal( b%minimum_radius__m( ), 1.0e-08_musica_dk ) )

    ! normalized radius
    call assert( 854389228, b%normalize_radius( 1.0e-09_musica_dk )           &
                            .eq. -1.0_musica_dk )
    call assert( 684232324,                                                   &
                 almost_equal( b%normalize_radius( 1.0e-07_musica_dk ),       &
                               0.0_musica_dk ) )
    call assert( 796550669, b%normalize_radius( 1.0e-05_musica_dk )           &
                            .eq. 1.0_musica_dk )

    ! get optics (absorption)
    ref_ind(2) = cmplx( 5.0_musica_dk, 60.0_musica_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 543590556, absorp(1,1) .eq. 0.0_musica_dk )
    call assert( 422649131, absorp(2,1) .eq. 0.0_musica_dk )
    call assert( 252492227, absorp(1,2) .eq. 4.0_musica_dk )
    call assert( 764868473, absorp(2,2) .eq. 7.0_musica_dk )
    call assert( 312236320, absorp(1,3) .eq. 0.0_musica_dk )
    call assert( 759604166, absorp(2,3) .eq. 0.0_musica_dk )

    ref_ind(2) = cmplx( 5.0_musica_dk, 50.0_musica_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 709234178, absorp(1,1) .eq. 0.0_musica_dk )
    call assert( 646131312, absorp(2,1) .eq. 0.0_musica_dk )
    call assert( 475974408, absorp(1,2) .eq. 3.0_musica_dk )
    call assert( 588292753, absorp(2,2) .eq. 6.0_musica_dk )
    call assert( 135660600, absorp(1,3) .eq. 0.0_musica_dk )
    call assert( 312987345, absorp(2,3) .eq. 0.0_musica_dk )

    ref_ind(2) = cmplx( 6.0_musica_dk, 60.0_musica_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 299099958, absorp(1,1) .eq.  0.0_musica_dk )
    call assert( 411418303, absorp(2,1) .eq.  0.0_musica_dk )
    call assert( 306269799, absorp(1,2) .eq.  9.0_musica_dk )
    call assert( 753637645, absorp(2,2) .eq. 14.0_musica_dk )
    call assert( 301005492, absorp(1,3) .eq.  0.0_musica_dk )
    call assert( 130848588, absorp(2,3) .eq.  0.0_musica_dk )

    ! interpolation tests
    ref_ind(2) = cmplx( 5.5_musica_dk, 60.0_musica_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 313726763, absorp(1,1) .eq.  0.0_musica_dk )
    call assert( 761094609, absorp(2,1) .eq.  0.0_musica_dk )
    call assert( 873412954, absorp(1,2) .eq.  6.5_musica_dk )
    call assert( 420780801, absorp(2,2) .eq. 10.5_musica_dk )
    call assert( 868148647, absorp(1,3) .eq.  0.0_musica_dk )
    call assert( 697991743, absorp(2,3) .eq.  0.0_musica_dk )

    ref_ind(2) = cmplx( 5.0_musica_dk, 55.0_musica_dk )
    call b%get_optics( ref_ind, absorption = absorp )

    call assert( 527834839, absorp(1,1) .eq. 0.0_musica_dk )
    call assert( 422686335, absorp(2,1) .eq. 0.0_musica_dk )
    call assert( 252529431, absorp(1,2) .eq. 3.5_musica_dk )
    call assert( 982372526, absorp(2,2) .eq. 6.5_musica_dk )
    call assert( 812215622, absorp(1,3) .eq. 0.0_musica_dk )
    call assert( 359583469, absorp(2,3) .eq. 0.0_musica_dk )

    !! lookup table w/ absorption, extinction, and asymmetry factor

    call config%from_file( "optics_lookup_c_config.json" )
    c = optics_lookup_t( config )

    ! check maximum radius
    call assert( 967771318,                                                   &
                 almost_equal( c%maximum_radius__m( ), 1.0e-06_musica_dk ) )

    ! check minimum radius
    call assert( 127515412,                                                   &
                 almost_equal( c%minimum_radius__m( ), 1.0e-08_musica_dk ) )

    ! normalized radius
    call assert( 122251105, c%normalize_radius( 1.0e-09_musica_dk )           &
                            .eq. -1.0_musica_dk )
    call assert( 517044699,                                                   &
                 almost_equal( c%normalize_radius( 1.0e-07_musica_dk ),       &
                               0.0_musica_dk ) )
    call assert( 511780392, c%normalize_radius( 1.0e-05_musica_dk )           &
                            .eq. 1.0_musica_dk )

    ! get optics (absorption)
    ref_ind(2) = cmplx( 5.0_musica_dk, 60.0_musica_dk )
    call c%get_optics( ref_ind, absorption = absorp, extinction = ext,        &
                       asymmetry_factor = asym )

    call assert( 622645498, absorp(1,1) .eq. 0.0_musica_dk )
    call assert( 170013345, absorp(2,1) .eq. 0.0_musica_dk )
    call assert( 899856440, absorp(1,2) .eq. 4.0_musica_dk )
    call assert( 447224287, absorp(2,2) .eq. 7.0_musica_dk )
    call assert( 342075783, absorp(1,3) .eq. 0.0_musica_dk )
    call assert( 171918879, absorp(2,3) .eq. 0.0_musica_dk )

    call assert( 724887524, ext(1,1) .eq. 0.0_musica_dk )
    call assert( 272255371, ext(2,1) .eq. 0.0_musica_dk )
    call assert( 719623217, ext(1,2) .eq. 7.0_musica_dk )
    call assert( 266991064, ext(2,2) .eq. 2.0_musica_dk )
    call assert( 161842560, ext(1,3) .eq. 0.0_musica_dk )
    call assert( 609210406, ext(2,3) .eq. 0.0_musica_dk )

    call assert( 721528751, asym(1,1) .eq.  0.0_musica_dk )
    call assert( 551371847, asym(2,1) .eq.  0.0_musica_dk )
    call assert( 998739693, asym(1,2) .eq.  9.0_musica_dk )
    call assert( 546107540, asym(2,2) .eq. 11.0_musica_dk )
    call assert( 440959036, asym(1,3) .eq.  0.0_musica_dk )
    call assert( 605851633, asym(2,3) .eq.  0.0_musica_dk )

  end subroutine test_optics_lookup_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_optics_lookup
