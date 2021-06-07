! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module shr_log_mod
  use shr_kind_mod,                    only : shr_kind_in, shr_kind_cx
  implicit none
  private
  public :: shr_log_errMsg
  integer(shr_kind_in), public :: shr_log_level = 0
  integer(shr_kind_in), public :: shr_log_unit  = 6
contains
  pure function shr_log_errMsg(file, line)
    character(shr_kind_cx)       :: shr_log_errMsg
    character(len=*), intent(in) :: file
    integer,          intent(in) :: line
    shr_log_errMsg = "error"//file
  end function shr_log_errMsg
end module shr_log_mod
