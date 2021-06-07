! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module shr_scam_mod
  implicit none
  private
  public :: shr_scam_getCloseLatLon
contains
  subroutine shr_scam_getCloseLatLon(pioid, targetLat,  targetLon, closeLat, &
      closeLon, closeLatIdx, closeLonIdx, found, rc )
    use pio
    use shr_kind_mod,                  only : R8 => shr_kind_r8,              &
                                              IN => shr_kind_in
    implicit none
    type(file_desc_t), intent(inout) :: pioid
    real   (R8),       intent(in)    :: targetLat
    real   (R8),       intent(in)    :: targetLon
    real   (R8),       intent(out)   :: closeLat
    real   (R8),       intent(out)   :: closeLon
    integer(IN),       intent(out)   :: closeLatIdx
    integer(IN),       intent(out)   :: closeLonIdx
    logical, optional, intent(out)   :: found
    integer, optional, intent(out)   :: rc
    if( present( found ) ) found = .true.
    if( present( rc    ) ) rc    = 0
    closeLat = 12.5_R8
    closeLon = 32.5_R8
    closeLatIdx = 3
    closeLonIdx = 5
  end subroutine shr_scam_getCloseLatLon
end module shr_scam_mod
