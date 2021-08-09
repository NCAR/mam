! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_constants module

!> Constants used in MAM calculations
module mam_constants

  use musica_constants,                only : musica_dk

  implicit none

  !> Water density at STP [kg m-3]
  real(kind=musica_dk), parameter :: kWaterDensitySTP = 1.0e3_musica_dk
  !> AccellerationByGravity [m s-2]
  real(kind=musica_dk), parameter :: kAccellerationByGravity =                &
                                     9.80616_musica_dk

end module mam_constants
