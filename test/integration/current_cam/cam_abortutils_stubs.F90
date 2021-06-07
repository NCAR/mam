! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module cam_abortutils
  implicit none
  private
  public :: endrun
contains
  subroutine endrun(error_msg, code)
    character(len=*), intent(in), optional :: error_msg
    integer,          intent(in), optional :: code
    if( present( error_msg ) ) write(*,*) error_msg
    write(*,*) "error"
    stop 3
  end subroutine endrun
end module cam_abortutils
