! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_host program

!> Mock host model - driver of integration tests
program test_mock_host

  use ai_aerosol,                      only : aerosol_t
  use ai_environmental_state,          only : environmental_state_t
  use mam_core,                        only : mam_core_t => core_t
  use test_mock_radiation,             only : rad_core_t => core_t

  implicit none

  integer, parameter :: kNumberOfLayersPerColumn = 32
  integer, parameter :: kNumberOfColumnsPerNode  = 150

  class(aerosol_t), pointer :: aerosol
  type(rad_core_t) :: rad_core
  type(environmental_state_t) :: environmental_state

  aerosol => mam_core_t( kNumberOfColumnsPerNode, kNumberOfLayersPerColumn )
  rad_core = rad_core_t( aerosol, kNumberOfColumnsPerNode,                    &
                         kNumberOfLayersPerColumn )

  ! do time loops

    call rad_core%run( aerosol, environmental_state )

  ! end time loops

end program test_mock_host
