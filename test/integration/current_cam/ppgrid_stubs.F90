! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module ppgrid
  implicit none
  private
  integer, parameter, public :: pcols = 1000     ! number of columns (max)
  integer, parameter, public :: pver  = 50       ! number of vertical levels
  integer, parameter, public :: pverp = pver + 1 ! pver + 1
end module ppgrid
