! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_species module

!> Test module for the species_t type and related functions
program test_species

  implicit none

  call test_species_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests for the \c species_t type
  subroutine test_species_t( )

    use mam_species,                   only : species_t
    use musica_assert,                 onlY : assert, almost_equal
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk

    type(species_t) :: species
    type(config_t)  :: config

    call config%from_file( "species_config.json" )
    species = species_t( config )

    call assert( 925498072, species%name( ) .eq. "foo" )

    call assert( 297444708, species%volume__m3( 100.0_dk )                    &
                            .eq. 100.0_dk / 814.0_dk )

    call assert( 180314351,                                                   &
                 almost_equal( species%shortwave_refractive_index( 1 ),       &
                               cmplx( 0.2_dk, 12.3_dk, kind = dk ) ) )
    call assert( 712754854,                                                   &
                 almost_equal( species%shortwave_refractive_index( 3 ),       &
                               cmplx( 1.0_dk, 3.2_dk, kind = dk ) ) )
    call assert( 825073199,                                                   &
                 almost_equal( species%shortwave_refractive_index( 4 ),       &
                               cmplx( 4.0_dk, 4.8_dk, kind = dk ) ) )
    call assert( 654916295,                                                   &
                 almost_equal( species%longwave_refractive_index( 1 ),        &
                               cmplx( 4.2_dk, 3.2_dk, kind = dk ) ) )
    call assert( 149709890,                                                   &
                 almost_equal( species%longwave_refractive_index( 3 ),        &
                               cmplx( -3.6_dk, 6.5_dk, kind = dk ) ) )
    call assert( 879552985,                                                   &
                 almost_equal( species%longwave_refractive_index( 6 ),        &
                               cmplx( 12.3_dk, 102.4_dk, kind = dk ) ) )

  end subroutine test_species_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_species
