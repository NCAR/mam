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
    type(optics_lookup_t) :: a
    type(config_t) :: config
    complex(kind=musica_dk) :: ref_ind( kNumberOfBands )

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

  end subroutine test_optics_lookup_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_optics_lookup
