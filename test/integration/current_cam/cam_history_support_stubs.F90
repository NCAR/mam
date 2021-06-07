! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module cam_history_support
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  implicit none
  private
  real(r8), public :: fillvalue = -9999999.0_r8
end module cam_history_support
