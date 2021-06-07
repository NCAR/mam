! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module physconst
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  implicit none
  private
  real(r8), public :: rga    = 1.0_r8 / 9.80616_r8 ! 1 / gravity accell.
  real(r8), public :: rhoh2o = 1.0e3_r8 ! water density @ STP kg/m3
  real(r8), public :: rair = 6.02214e26_r8 * 1.38065e-23_r8 / 28.996_r8 ! dry air gas constant J/K/kg
end module physconst
