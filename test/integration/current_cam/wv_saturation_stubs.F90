! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module wv_saturation
  implicit none
  private
  public :: qsat
  interface qsat
    module procedure :: qsat_line
    module procedure :: qsat_vect
  end interface
contains
  subroutine qsat_line(t, p, es, qs)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    real(r8), intent(in) :: t ! temperature
    real(r8), intent(in) :: p ! pressure
    real(r8), intent(out) :: es ! saturation vapor pressure
    real(r8), intent(out) :: qs ! saturation specific humidity
    es = 2065.0_r8
    qs = 0.02_r8
  end subroutine qsat_line
  subroutine qsat_vect(t, p, es, qs, vlen, gam, dqsdt, enthalpy)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    integer,                   intent(in) :: vlen
    real(r8), dimension(vlen), intent(in) :: t    ! Temperature
    real(r8), dimension(vlen), intent(in) :: p    ! Pressure
    real(r8), dimension(vlen), intent(out) :: es  ! Saturation vapor pressure
    real(r8), dimension(vlen), intent(out) :: qs  ! Saturation specific humidity
    real(r8), dimension(vlen), intent(out), optional :: gam ! (l/cpair)*(d(qs)/dt)
    real(r8), dimension(vlen), intent(out), optional :: dqsdt ! (d(qs)/dt)
    real(r8), dimension(vlen), intent(out), optional :: enthalpy ! cpair*t + hltalt*q
    es(:) = 2065.0_r8
    qs(:) = 0.02_r8
  end subroutine qsat_vect
end module wv_saturation
