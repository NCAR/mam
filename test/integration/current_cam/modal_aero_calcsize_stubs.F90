! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module modal_aero_calcsize

  use ppgrid,                          only : pcols, pver
  use shr_kind_mod,                    only : r8 => shr_kind_r8

  implicit none
  private

  public :: modal_aero_calcsize_init, modal_aero_calcsize_diag

  integer, parameter :: nmodes = 4

  ! state data
  real(r8), dimension(pcols,pver,nmodes) :: dgnum     ! interstitial aerosol dry number mode radius [m]
  real(r8), dimension(pcols,pver,nmodes) :: hygro     ! volume-weighted mean hygroscopicity [-]
  real(r8), dimension(pcols,pver,nmodes) :: dryvol    ! single-particle-mean dry volume [m3]
  real(r8), dimension(pcols,pver,nmodes) :: dryrad    ! dry volume mean radius [m]
  real(r8), dimension(pcols,pver,nmodes) :: drymass   ! single-particle-mean dry mass [kg]
  real(r8), dimension(pcols,pver,nmodes) :: so4dryvol ! single-particle-mean so4 dry volume [m3]
  reaL(r8), dimension(pcols,pver,nmodes) :: naer      ! aerosol number mixing ratio [# kg_air-1]

contains

  subroutine modal_aero_calcsize_init( )
    use test_utils,                    only : set_values
    call set_values( dgnum,    1.0e-8_r8, 0.5_r8 )
    call set_values( hygro,    1.0e-5_r8, 0.4_r8 )
    call set_values( dryvol,  4.2e-24_r8, 0.5_r8 )
    call set_values( dryrad,  11.0e-8_r8, 0.5_r8 )
    call set_values( drymass, 4.2e-21_r8, 0.5_r8 )
    so4dryvol = dryvol * 0.4
    call set_values( naer,      1.0e7_r8, 0.5_r8 )
  end subroutine modal_aero_calcsize_init

  subroutine modal_aero_calcsize_diag(state, pbuf, list_idx_in, dgnum_m,      &
      hygro_m, dryvol_m, dryrad_m, drymass_m, so4dryvol_m, naer_m)
    use physics_buffer,                only : physics_buffer_desc
    use physics_types,                 only : physics_state
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    type(physics_state), intent(in), target :: state
    type(physics_buffer_desc), pointer :: pbuf(:)
    integer,  optional, intent(in)   :: list_idx_in
    real(r8), optional, pointer      :: dgnum_m(:,:,:)
    real(r8), optional, pointer      :: hygro_m(:,:,:)
    real(r8), optional, pointer      :: dryvol_m(:,:,:)
    real(r8), optional, pointer      :: dryrad_m(:,:,:)
    real(r8), optional, pointer      :: drymass_m(:,:,:)
    real(r8), optional, pointer      :: so4dryvol_m(:,:,:)
    real(r8), optional, pointer      :: naer_m(:,:,:)
    if( present( dgnum_m     ) ) dgnum_m(:,:,:)     = dgnum(:,:,:)
    if( present( hygro_m     ) ) hygro_m(:,:,:)     = hygro(:,:,:)
    if( present( dryvol_m    ) ) dryvol_m(:,:,:)    = dryvol(:,:,:)
    if( present( dryrad_m    ) ) dryrad_m(:,:,:)    = dryrad(:,:,:)
    if( present( drymass_m   ) ) drymass_m(:,:,:)   = drymass(:,:,:)
    if( present( so4dryvol_m ) ) so4dryvol_m(:,:,:) = so4dryvol(:,:,:)
    if( present( naer_m      ) ) naer_m(:,:,:)      = naer(:,:,:)
  end subroutine modal_aero_calcsize_diag

end module modal_aero_calcsize
