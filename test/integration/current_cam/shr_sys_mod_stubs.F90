! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module shr_sys_mod
  use cam_abortutils,                  only : shr_sys_abort => endrun
  implicit none
  private
  public :: shr_sys_flush, shr_sys_abort, shr_sys_chdir, shr_sys_system
contains
  subroutine shr_sys_flush(unit)
    use shr_kind_mod,                  only : shr_kind_in
    implicit none
    integer(shr_kind_in), intent(in) :: unit
    flush(unit)
  end subroutine shr_sys_flush
  subroutine shr_sys_chdir(path, rcode)
    use shr_kind_mod,                  only : shr_kind_in
    character(len=*), intent(in) :: path
    integer(shr_kind_in), intent(out) :: rcode
    rcode = chdir( trim( path ) )
  end subroutine
  subroutine shr_sys_system(str, rcode)
    use shr_kind_mod,                  only : shr_kind_in
    character(len=*), intent(in) :: str
    integer(shr_kind_in), intent(out) :: rcode
    call execute_command_line( trim( str ), exitstat = rcode )
  end subroutine shr_sys_system
end module shr_sys_mod
