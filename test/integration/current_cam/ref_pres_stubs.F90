! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module ref_pres
  use ppgrid,                          only : pver
  implicit none
  private
  integer, parameter, public :: clim_modal_aero_top_lev = 4
end module ref_pres
