! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module scamMod
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  implicit none
  private
  real(r8), parameter, public :: scmlat        = 12.5_r8
  real(r8), parameter, public :: scmlon        = 32.5_r8
  logical,  parameter, public :: single_column = .false.
end module scamMod
