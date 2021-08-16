! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_cam program

!> Mock CAM model - driver of comparison with original CAM aerosol
!! optics code
program test_mock_cam

  use ai_accessor,                     only : accessor_t
  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use ai_constants,                    only : kDouble
  use ai_environmental_state,          only : environmental_state_t
  use ai_optics,                       only : optics_t
  use cam_abortutils,                  only : endrun
  use aer_rad_props,                   only : aer_rad_props_init,             &
                                              aer_rad_props_sw,               &
                                              aer_rad_props_lw
  use cam_pio_utils,                   only : init_pio_subsystem
  use mam_core,                        only : mam_core_t => core_t
  use modal_aer_opt,                   only : modal_aer_opt_readnl,           &
                                              modal_aer_opt_init
  use modal_aero_calcsize,             only : modal_aero_calcsize_init
  use modal_aero_wateruptake,          only : modal_aero_wateruptake_init
  use mpi
  use musica_config,                   only : config_t
  use physics_types,                   only : physics_state
  use physics_buffer,                  only : physics_buffer_desc, pbuf_init
  use ppgrid,                          only : pcols, pver
  use rad_constituents,                only : rad_cnst_readnl, rad_cnst_init
  use radconstants,                    only : nswbands, nlwbands
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  use shr_pio_mod,                     only : shr_pio_init1, shr_pio_init2

  implicit none

  integer, parameter :: kShortTau = 1
  integer, parameter :: kShortWa  = 2
  integer, parameter :: kShortGa  = 3
  integer, parameter :: kShortFa  = 4
  integer, parameter :: kLongAbs  = 1

  class(aerosol_t),            pointer :: aerosol
  class(aerosol_state_t),      pointer :: aerosol_state
  type(environmental_state_t)          :: environmental_state
  type(config_t)                       :: mam_config
  class(accessor_t),           pointer :: sw_accessor, lw_accessor
  type(optics_t)                       :: sw_optics, lw_optics

  integer                            :: list_idx = 0 ! indicator of a diagnostic? 0 seems to be the regular calculation
  type(physics_state),       target  :: state
  type(physics_buffer_desc), pointer :: pbuf(:)
  integer                            :: nnite          ! number of night columns
  integer                            :: idxnite(pcols) ! local column indices of night columns

  real(r8) :: tau    (pcols,0:pver,nswbands) ! aerosol extinction optical depth
  real(r8) :: tau_w  (pcols,0:pver,nswbands) ! aerosol single scattering albedo * tau
  real(r8) :: tau_w_g(pcols,0:pver,nswbands) ! aerosol assymetry parameter * tau * w
  real(r8) :: tau_w_f(pcols,0:pver,nswbands) ! aerosol forward scattered fraction * tau * w

  real(r8) :: odap_aer(pcols,pver,nlwbands) ! [fraction] absorption optical depth, per layer

  integer, dimension(1) :: comp_id, comp_comm, comp_comm_iam
  character(len=20), dimension(1) :: comp_name
  logical, dimension(1) :: comp_iamin

  integer :: i_column, i_layer, error_code, compute_comm


  !! Current CAM code initialization

  comp_id(1)       = 0
  comp_comm(1)     = MPI_COMM_WORLD
  comp_comm_iam(1) = 0
  comp_name(1)     = "foo"
  comp_iamin(1)    = .true.

  call mpi_init( error_code )
  if ( error_code .ne. MPI_SUCCESS ) call endrun( "MPI init error", error_code )
  compute_comm = MPI_COMM_WORLD
  call shr_pio_init1( 8, "drv_in", compute_comm )
  call shr_pio_init2( comp_id, comp_name, comp_iamin, comp_comm, comp_comm_iam )

  call init_pio_subsystem( )
  call rad_cnst_readnl( 'atm_in' )
  call rad_cnst_init( )

  call modal_aer_opt_readnl( 'atm_in' )
  call modal_aer_opt_init( )

  call modal_aero_calcsize_init( )

  state = physics_state( pcols, pver, 4 + 6 + 4 + 3 + 2 ) ! the last dimension is the number of constituents
                                                          ! (number mr and species for each mode)

  call pbuf_init( pbuf )

  nnite = 0
  do i_column = 1, pcols
    idxnite( i_column ) = i_column
  end do

  !! MAM initialization

  call mam_config%from_file( "mam_data/mam_config.json" )
  aerosol => mam_core_t( mam_config )
  aerosol_state => aerosol%get_new_state( )
  call set_up_optics( sw_optics, lw_optics )
  sw_accessor => aerosol%optics_accessor( sw_optics )
  lw_accessor => aerosol%optics_accessor( lw_optics )

  !! Current CAM code run

  call aer_rad_props_init( )
  call aer_rad_props_sw( list_idx, state, pbuf, nnite, idxnite, tau, tau_w, tau_w_g, tau_w_f )
  call aer_rad_props_lw( list_idx, state, pbuf, odap_aer )

  !! MAM run

  do i_column = 1, pcols
    do i_layer = 1, pver
      call set_mam_states( i_column, i_layer, pbuf, state, aerosol_state, environmental_state )
      call aerosol%get_optics( sw_accessor, environmental_state, aerosol_state, sw_optics )
      call aerosol%get_optics( lw_accessor, environmental_state, aerosol_state, lw_optics )
      if( i_column .eq. 13 .and. i_layer .eq. 26 ) then
        write(*,*) "MAM results"
        write(*,*) "tau"
        write(*,*) sw_optics%values_(:, kShortTau )
        write(*,*) "tau_w"
        write(*,*) sw_optics%values_(:, kShortWa )
        write(*,*) "tau_w_g"
        write(*,*) sw_optics%values_(:, kShortGa )
        write(*,*) "tau_w_f"
        write(*,*) sw_optics%values_(:, kShortFa )
        write(*,*) "odap_aer"
        write(*,*) sw_optics%values_(:, kLongAbs )
      end if
    end do
  end do

  write(*,*) "CAM results"
  write(*,*) "tau"
  write(*,*) tau(13,26,:)
  write(*,*) "tau_w"
  write(*,*) tau_w(13,26,:)
  write(*,*) "tau_w_g"
  write(*,*) tau_w_g(13,26,:)
  write(*,*) "tau_w_f"
  write(*,*) tau_w_f(13,26,:)
  write(*,*) "odap_aer"
  write(*,*) odap_aer(13,26,:)

  deallocate( sw_accessor )
  deallocate( lw_accessor )

contains

  subroutine set_up_optics( sw_optics, lw_optics )

    use ai_wavelength_grid,            only : wavelength_grid_t,              &
                                              kWavenumber, kCentimeter
    use ai_property,                   only : property_t
    use radconstants,                  only : wavenum_low, wavenum_high,      &
                                              wavenumber1_longwave,           &
                                              wavenumber2_longwave

    type(optics_t), intent(inout) :: sw_optics
    type(optics_t), intent(inout) :: lw_optics

    type(wavelength_grid_t)        :: sw_grid, lw_grid
    type(property_t), dimension(4) :: sw_props
    type(property_t), dimension(1) :: lw_props

    sw_grid = wavelength_grid_t( wavenum_low, wavenum_high,                   &
                                 bounds_in = kWavenumber,                     &
                                 base_unit = kCentimeter )
    lw_grid = wavelength_grid_t( wavenumber1_longwave, wavenumber2_longwave,  &
                                 bounds_in = kWavenumber,                     &
                                 base_unit = kCentimeter )
    sw_props(kShortTau) =                                                     &
        property_t( "layer extinction optical depth",    "unitless" )
    sw_props(kShortWa)  =                                                     &
        property_t( "layer single-scatter albedo depth", "unitless" )
    sw_props(kShortGa)  =                                                     &
        property_t( "asymmetry factor",                  "unitless" )
    sw_props(kShortFa)  =                                                     &
        property_t( "forward scattered fraction",        "unitless" )
    lw_props(kLongAbs)  =                                                     &
        property_t( "layer absorption optical depth",    "unitless" )
    sw_optics = optics_t( sw_props, sw_grid )
    lw_optics = optics_t( lw_props, lw_grid )

  end subroutine set_up_optics

  subroutine set_mam_states( i_column, i_layer, pbuf, state, aerosol_state,   &
      environmental_state )

    use musica_assert,                 only : assert
    use physics_buffer,                only : pbuf_get_index, pbuf_get_field

    integer,                            intent(in)    :: i_column
    integer,                            intent(in)    :: i_layer
    type(physics_buffer_desc), pointer, intent(inout) :: pbuf(:)
    class(physics_state),      target,  intent(in)    :: state
    class(aerosol_state_t),             intent(inout) :: aerosol_state
    class(environmental_state_t),       intent(inout) :: environmental_state

    integer                :: pbuf_id, errcode
    real(kind=r8), pointer :: pbuf_array(:,:,:)
    real(kind=r8)          :: dgnumwet(4), qaerwat(4), mam_state(27)

    pbuf_id = -1
    pbuf_id = pbuf_get_index('DGNUMWET', errcode)
    call assert( 343719600, errcode .eq. 0 )
    call pbuf_get_field( pbuf, pbuf_id, pbuf_array )
    dgnumwet = pbuf_array( i_column, i_layer, : )

    pbuf_id = -1
    pbuf_id = pbuf_get_index('QAERWAT',  errcode)
    call assert( 894782711, errcode .eq. 0 )
    call pbuf_get_field( pbuf, pbuf_id, pbuf_array )
    qaerwat  = pbuf_array( i_column, i_layer, : )

    mam_state(1)     = dgnumwet(1)
    mam_state(2:8)   = state%q( i_column, i_layer, 1:7 )
    mam_state(9)     = qaerwat(1)
    mam_state(10)    = dgnumwet(2)
    mam_state(11:15) = state%q( i_column, i_layer, 8:12 )
    mam_state(16)    = qaerwat(2)
    mam_state(17)    = dgnumwet(3)
    mam_state(18:21) = state%q( i_column, i_layer, 13:16 )
    mam_state(22)    = qaerwat(3)
    mam_state(23)    = dgnumwet(4)
    mam_state(24:26) = state%q( i_column, i_layer, 17:19 )
    mam_state(27)    = qaerwat(4)

    call environmental_state%set_layer_thickness__Pa(                         &
                                          state%pdeldry( i_column, i_layer ) )
    call aerosol_state%load_state( mam_state )

  end subroutine set_mam_states

end program test_mock_cam
