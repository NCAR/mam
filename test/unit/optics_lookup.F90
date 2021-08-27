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
    use musica_config,                 only : config_t

    type(optics_lookup_t) :: a
    type(config_t) :: config

    call config%from_file( "optics_lookup_a_config.json" )
    a = optics_lookup_t( config )

  end subroutine test_optics_lookup_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_optics_lookup
