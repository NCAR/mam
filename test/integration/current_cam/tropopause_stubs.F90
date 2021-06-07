! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module tropopause

  implicit none
  private

  public :: tropopause_find, tropopause_findChemTrop

contains

  subroutine tropopause_find(pstate, tropLev, tropP, tropT, tropZ, primary,   &
      backup)
    use physics_types,                 only : physics_state
    use ppgrid,                        only : pcols
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    implicit none
    type(physics_state), intent(in) :: pstate
    integer, optional, intent(in)   :: primary ! primary detection algorithm
    integer, optional, intent(in)   :: backup  ! backup detection algorithm
    integer,            intent(out) :: tropLev(pcols) ! tropopause level index
    real(r8), optional, intent(out) :: tropP(pcols) ! tropopause pressure (Pa)
    real(r8), optional, intent(out) :: tropT(pcols) ! tropopause temperature (K)
    real(r8), optional, intent(out) :: tropZ(pcols) ! tropopause height (m)
    tropLev(:) = 10
    tropP(:)   = 90000.0_r8
    tropT(:)   = 195.0_r8
    tropZ(:)   = 20000.0_r8
  end subroutine tropopause_find

  subroutine tropopause_findChemTrop(pstate, tropLev, primary, backup)
    use physics_types,                 only : physics_state
    use ppgrid,                        only : pcols
    implicit none
    type(physics_state), intent(in) :: pstate
    integer, optional, intent(in)   :: primary ! primary detection algorithm
    integer, optional, intent(in)   :: backup  ! backup detection algorithm
    integer,            intent(out) :: tropLev(pcols) ! tropopause level index
    tropLev(:) = 10
  end subroutine tropopause_findChemTrop

end module tropopause
