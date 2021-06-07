! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module phys_control
  implicit none
  private
  public :: phys_getopts
  logical, parameter, public :: use_simple_phys = .false.
contains
  subroutine phys_getopts(history_amwg_out, history_aero_optics_out,          &
      history_dust_out, prog_modal_aero_out)
    logical, optional, intent(out) :: history_amwg_out
    logical, optional, intent(out) :: history_aero_optics_out
    logical, optional, intent(out) :: history_dust_out
    logical, optional, intent(out) :: prog_modal_aero_out
    if( present( history_amwg_out        ) ) history_amwg_out        = .false.
    if( present( history_aero_optics_out ) ) history_aero_optics_out = .false.
    if( present( history_dust_out        ) ) history_dust_out        = .false.
    if( present( prog_modal_aero_out     ) ) prog_modal_aero_out     = .false.
  end subroutine phys_getopts
end module phys_control
