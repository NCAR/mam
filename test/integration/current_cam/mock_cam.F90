! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_cam program

!> Mock CAM model - driver of comparison with original CAM aerosol
!! optics code
program test_mock_cam

  use cam_abortutils,                  only : endrun
  use aer_rad_props,                   only : aer_rad_props_init,             &
                                              aer_rad_props_sw,               &
                                              aer_rad_props_lw
  use cam_pio_utils,                   only : init_pio_subsystem
  use modal_aer_opt,                   only : modal_aer_opt_readnl,           &
                                              modal_aer_opt_init
  use modal_aero_calcsize,             only : modal_aero_calcsize_init
  use modal_aero_wateruptake,          only : modal_aero_wateruptake_init
  use mpi
  use physics_types,                   only : physics_state
  use physics_buffer,                  only : physics_buffer_desc, pbuf_init
  use ppgrid,                          only : pcols, pver
  use rad_constituents,                only : rad_cnst_readnl, rad_cnst_init
  use radconstants,                    only : nswbands, nlwbands
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  use shr_pio_mod,                     only : shr_pio_init1, shr_pio_init2

  implicit none

  integer                            :: list_idx = 0 ! indicator of a diagnostic? 0 seems to be the regular calculation
  type(physics_state),       target  :: state
  type(physics_buffer_desc), pointer :: pbuf(:)
  integer                            :: nnite          ! number of night columns
  integer                            :: idxnite(pcols) ! local column indices of night columns

  real(r8) :: tau    (pcols,0:pver,nswbands) ! aerosol extinction optical depth
  real(r8) :: tau_w  (pcols,0:pver,nswbands) ! aerosol single scattering albedo * tau
  real(r8) :: tau_w_g(pcols,0:pver,nswbands) ! aerosol assymetry parameter * tau * w
  real(r8) :: tau_w_f(pcols,0:pver,nswbands) ! aerosol forward scattered fraction * tau * w

  real(r8) :: odap_aer(pcols,pver,nlwbands) ! [fraction] absorption optical depth, per layer

  integer, dimension(1) :: comp_id, comp_comm, comp_comm_iam
  character(len=20), dimension(1) :: comp_name
  logical, dimension(1) :: comp_iamin

  integer :: i_col, error_code, compute_comm

  comp_id(1)       = 0
  comp_comm(1)     = MPI_COMM_WORLD
  comp_comm_iam(1) = 0
  comp_name(1)     = "foo"
  comp_iamin(1)    = .true.

  call mpi_init( error_code )
  if ( error_code .ne. MPI_SUCCESS ) call endrun( "MPI init error", error_code )
  compute_comm = MPI_COMM_WORLD
  call shr_pio_init1( 8, "drv_in", compute_comm )
  call shr_pio_init2( comp_id, comp_name, comp_iamin, comp_comm, comp_comm_iam )

  call init_pio_subsystem( )
  call rad_cnst_readnl( 'atm_in' )
  call rad_cnst_init( )

  call modal_aer_opt_readnl( 'atm_in' )
  call modal_aer_opt_init( )

  call modal_aero_calcsize_init( )

  state = physics_state( pcols, pver, 1 ) ! the last dimension is the number of water species
  call pbuf_init( )

  nnite = 0
  do i_col = 1, pcols
    idxnite( i_col ) = i_col
  end do

  call aer_rad_props_init( )
  call aer_rad_props_sw( list_idx, state, pbuf, nnite, idxnite, tau, tau_w, tau_w_g, tau_w_f )
  call aer_rad_props_lw( list_idx, state, pbuf, odap_aer )

  write(*,*) "tau"
  write(*,*) tau(3,3,:)

end program test_mock_cam
