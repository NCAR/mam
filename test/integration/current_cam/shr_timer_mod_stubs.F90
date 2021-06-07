! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module shr_timer_mod
  use shr_kind_mod,                    only : shr_kind_in
  implicit none
  private
  public :: shr_timer_get, shr_timer_start, shr_timer_stop
contains
  subroutine shr_timer_get(n, str)
    integer(shr_kind_in), intent(out) :: n
    character(len=*),     intent(in)  :: str
    n = 1
  end subroutine
  subroutine shr_timer_start(n)
    integer(shr_kind_in), intent(in) :: n
  end subroutine shr_timer_start
  subroutine shr_timer_stop(n)
    integer(shr_kind_in), intent(in) :: n
  end subroutine
end module shr_timer_mod
