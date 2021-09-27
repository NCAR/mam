! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_host program

!> Mock host model - driver of integration tests
program test_mock_host

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use ai_environmental_state,          only : environmental_state_t
  use mam_core,                        only : mam_core_t => core_t
  use musica_config,                   only : config_t
  use musica_constants,                only : musica_dk
  use test_mock_radiation,             only : rad_core_t => core_t

  implicit none

  integer, parameter :: kNumberOfLayersPerColumn = 32
  integer, parameter :: kNumberOfColumnsPerNode  = 150

  class(aerosol_t), pointer :: aerosol
  class(aerosol_state_t), pointer :: aerosol_state
  real(kind=musica_dk), allocatable :: raw_aerosol_states(:,:,:)
  type(rad_core_t) :: rad_core
  type(environmental_state_t), allocatable :: environmental_states(:,:)
  type(config_t) :: mam_config
  integer :: i_column, i_layer

  call mam_config%from_file( "mam_config.json" )
  aerosol => mam_core_t( mam_config )
  aerosol_state => aerosol%new_state( )
  allocate( raw_aerosol_states( aerosol_state%raw_size( ),                    &
                                kNumberOfLayersPerColumn,                     &
                                kNumberOfColumnsPerNode ) )

  allocate( environmental_states( kNumberOfLayersPerColumn,                   &
                                  kNumberOfColumnsPerNode ) )

  ! initialize the model state to random values
  do i_column = 1, kNumberOfColumnsPerNode
    do i_layer = 1, kNumberOfLayersPerColumn
      call environmental_states( i_layer, i_column )%randomize( )
      associate( raw_state => raw_aerosol_states( :, i_layer, i_column ) )
        call aerosol_state%load_state( raw_state )
        call aerosol_state%randomize( )
        call aerosol_state%dump_state( raw_state )
      end associate
    end do
  end do

  rad_core = rad_core_t( aerosol, kNumberOfColumnsPerNode,                    &
                         kNumberOfLayersPerColumn )

  ! do time loops

    call rad_core%run( aerosol, aerosol_state, raw_aerosol_states,            &
                       environmental_states )

  ! end time loops

  deallocate( aerosol )
  deallocate( aerosol_state )
  deallocate( raw_aerosol_states )
  deallocate( environmental_states )

end program test_mock_host
