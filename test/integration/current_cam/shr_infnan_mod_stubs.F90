! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module shr_infnan_mod
  use, intrinsic :: ieee_arithmetic,   only : shr_infnan_isnan => ieee_is_nan
  implicit none
  private
  public :: shr_infnan_isnan
end module shr_infnan_mod
