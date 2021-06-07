! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module radconstants
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  implicit none
  private
  public :: rad_gas_index
  integer, parameter, public :: nswbands = 14
  real(r8),parameter, public :: wavenum_low(nswbands) = & ! in cm^-1
    (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8,   &
      8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
  real(r8),parameter, public :: wavenum_high(nswbands) = & ! in cm^-1
    (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8,   &
     12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)
  integer, parameter, public :: nlwbands = 16
  ! Longwave spectral band limits (cm-1)
  real(r8), parameter, public :: wavenumber1_longwave(nlwbands) = &
    (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8,  &
       1080._r8, 1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8,  &
       2390._r8, 2600._r8 /)
  ! Longwave spectral band limits (cm-1)
  real(r8), parameter, public :: wavenumber2_longwave(nlwbands) = &
    (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8,  &
       1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8,  &
       2600._r8, 3250._r8 /)
  ! number of evenly spaced intervals in rh
  integer, parameter, public :: nrh = 1000
  ! index to sw visible band
  integer, parameter, public :: idx_sw_diag = 10
  ! index to sw near infrared (778-1240 nm) band
  integer, parameter, public :: idx_nir_diag = 8
  ! index to sw uv (345-441 nm) band
  integer, parameter, public :: idx_uv_diag = 11
  ! length of "optics type" string specified in optics file
  integer, parameter, public :: ot_length = 32

  ! gasses required by radiation
  integer, public, parameter :: gasnamelength = 5
  integer, public, parameter :: nradgas = 8
  character(len=gasnamelength), public, parameter :: gaslist(nradgas) &
    = (/'H2O  ','O3   ', 'O2   ', 'CO2  ', 'N2O  ', 'CH4  ', 'CFC11', 'CFC12'/)
contains
  integer function rad_gas_index(gasname)
    character(len=*), intent(in) :: gasname
    rad_gas_index = 1
  end function rad_gas_index
end module radconstants
