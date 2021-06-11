! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module modal_aero_wateruptake

  use ppgrid,                          only : pcols, pver
  use shr_kind_mod,                    only : r8 => shr_kind_r8

  implicit none
  private

  public :: modal_aero_wateruptake_dr

  integer, parameter :: nmodes = 4

  real(r8), dimension(pcols,pver,nmodes), target :: dgnumdry
  real(r8), dimension(pcols,pver,nmodes), target :: dgnumwet
  real(r8), dimension(pcols,pver,nmodes), target :: qaerwat
  real(r8), dimension(pcols,pver,nmodes), target :: wetdens
  real(r8), dimension(pcols,pver,nmodes), target :: hygro
  real(r8), dimension(pcols,pver,nmodes), target :: dryvol
  real(r8), dimension(pcols,pver,nmodes), target :: dryrad
  real(r8), dimension(pcols,pver,nmodes), target :: drymass
  real(r8), dimension(pcols,pver,nmodes), target :: so4dryvol
  real(r8), dimension(pcols,pver,nmodes), target :: naer

contains

  subroutine modal_aero_wateruptake_dr(state, pbuf, list_idx_in, dgnumdry_m,  &
      dgnumwet_m, qaerwat_m, wetdens_m, hygro_m, dryvol_m, dryrad_m,          &
      drymass_m, so4dryvol_m, naer_m)
    use physics_buffer,                only : physics_buffer_desc
    use physics_types,                 only : physics_state
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    implicit none
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

    dgnumdry( :,:,:) = 1.0_r8
    dgnumwet( :,:,:) = 1.0_r8
    qaerwat(  :,:,:) = 1.0_r8
    wetdens(  :,:,:) = 1.0_r8
    hygro(    :,:,:) = 1.0_r8
    dryvol(   :,:,:) = 1.0_r8
    dryrad(   :,:,:) = 1.0_r8
    drymass(  :,:,:) = 1.0_r8
    so4dryvol(:,:,:) = 1.0_r8
    naer(     :,:,:) = 1.0_r8

    dgnumdry_m  => dgnumdry
    dgnumwet_m  => dgnumwet
    qaerwat_m   => qaerwat
    wetdens_m   => wetdens
    hygro_m     => hygro
    dryvol_m    => dryvol
    dryrad_m    => dryrad
    drymass_m   => drymass
    so4dryvol_m => so4dryvol
    naer_m      => naer

    write(*,*) "dgnumwet associated? ", associated( dgnumwet_m )
    write(*,*) "dgnumwet ", dgnumwet_m

  end subroutine modal_aero_wateruptake_dr

end module modal_aero_wateruptake
