! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module constituents
  implicit none
  private
  public :: cnst_get_ind
  integer, parameter, public :: pcnst = 100 ! number of advected constituents
contains
  subroutine cnst_get_ind(name, ind, abort)
    character(len=*),  intent(in)  :: name
    integer,           intent(out) :: ind
    logical, optional, intent(in)  :: abort
    ind = 1
  end subroutine cnst_get_ind
end module constituents
