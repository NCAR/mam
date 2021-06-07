! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_cam program

!> Mock CAM model - driver of comparison with original CAM aerosol
!! optics code
program test_mock_cam

  use aer_rad_props,                   only : aer_rad_props_init,             &
                                              aer_rad_props_sw,               &
                                              aer_rad_props_lw
  use physics_types,                   only : physics_state
  use physics_buffer,                  only : physics_buffer_desc
  use ppgrid,                          only : pcols, pver
  use radconstants,                    only : nswbands, nlwbands
  use shr_kind_mod,                    only : r8 => shr_kind_r8

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

  call aer_rad_props_init( )
  call aer_rad_props_sw( list_idx, state, pbuf, nnite, idxnite, tau, tau_w, tau_w_g, tau_w_f )
  call aer_rad_props_lw( list_idx, state, pbuf, odap_aer )

  write(*,*) "tau"
  write(*,*) tau(3,3,:)

end program test_mock_cam
