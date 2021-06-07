! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module namelist_utils
  use shr_nl_mod, only: find_group_name => shr_nl_find_group_name
  implicit none
  private
  public :: find_group_name
end module namelist_utils
