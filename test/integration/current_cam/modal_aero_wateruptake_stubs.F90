! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module modal_aero_wateruptake

  implicit none
  private

  public :: modal_aero_wateruptake_dr

contains

  subroutine modal_aero_wateruptake_dr(state, pbuf, list_idx_in, dgnumdry_m,  &
      dgnumwet_m, qaerwat_m, wetdens_m, hygro_m, dryvol_m, dryrad_m,          &
      drymass_m, so4dryvol_m, naer_m)
    use physics_buffer,                only : physics_buffer_desc
    use physics_types,                 only : physics_state
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    type(physics_state), target, intent(in)    :: state
    type(physics_buffer_desc),   pointer       :: pbuf(:)
    integer,  optional,          intent(in)    :: list_idx_in
    real(r8), optional,          pointer       :: dgnumdry_m(:,:,:)
    real(r8), optional,          pointer       :: dgnumwet_m(:,:,:)
    real(r8), optional,          pointer       :: qaerwat_m(:,:,:)
    real(r8), optional,          pointer       :: wetdens_m(:,:,:)
    real(r8), optional,          pointer       :: hygro_m(:,:,:)
    real(r8), optional,          pointer       :: dryvol_m(:,:,:)
    real(r8), optional,          pointer       :: dryrad_m(:,:,:)
    real(r8), optional,          pointer       :: drymass_m(:,:,:)
    real(r8), optional,          pointer       :: so4dryvol_m(:,:,:)
    real(r8), optional,          pointer       :: naer_m(:,:,:)
  end subroutine modal_aero_wateruptake_dr

end module modal_aero_wateruptake
