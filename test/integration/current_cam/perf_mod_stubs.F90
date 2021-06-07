! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module perf_mod

  implicit none
  private

  public :: t_startf, t_stopf

contains

  subroutine t_startf( event, handle )
    character(len=*), intent(in) :: event
    integer, optional :: handle
  end subroutine t_startf

  subroutine t_stopf( event, handle )
    character(len=*), intent(in) :: event
    integer, optional :: handle
  end subroutine t_stopf

end module perf_mod
