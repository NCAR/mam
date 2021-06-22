! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_constants module

!> Constants used to calculate MAM optical properties
module mam_optics_constants

  use ai_constants,                    only : r8 => kDouble

  ! Shortwave and longwave spectral bounds (cm-1)
  real(r8), parameter :: shortwave_lower(14) =                                &
    (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8,   &
      8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
  real(r8), parameter :: shortwave_upper(14) =                                &
    (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8,   &
     12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)
  real(r8), parameter :: longwave_lower(16) =                                 &
    (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8,  &
       1080._r8, 1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8,  &
       2390._r8, 2600._r8 /)
  real(r8), parameter :: longwave_upper(16) =                                 &
    (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8,  &
       1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8,  &
       2600._r8, 3250._r8 /)

end module mam_optics_constants
