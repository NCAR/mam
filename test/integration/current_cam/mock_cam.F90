! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_cam program

!> Mock CAM model - driver of comparison with original CAM aerosol
!! optics code
program test_mock_cam

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use ai_environmental_state,          only : environmental_state_t
  use ai_optics,                       only : optics_t, optics_ptr
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
  use musica_assert,                   only : assert, almost_equal
  use musica_config,                   only : config_t
  use musica_constants,                only : musica_dk
  use physics_types,                   only : physics_state
  use physics_buffer,                  only : physics_buffer_desc, pbuf_init
  use ppgrid,                          only : pcols, pver
  use rad_constituents,                only : rad_cnst_readnl, rad_cnst_init
  use radconstants,                    only : nswbands, nlwbands
  use ref_pres,                        only : top_lev => clim_modal_aero_top_lev
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
  class(optics_t),             pointer :: sw_extinction, sw_single_scatter_albedo
  class(optics_t),             pointer :: sw_asymmetry_factor, sw_forward_scattered_fraction
  class(optics_t),             pointer :: lw_absorption
  type(optics_ptr)                     :: sw_optics(4)
  real(r8),                    target  :: raw_mam_state(pcols,pver,27)

  integer                            :: list_idx = 0 ! indicator of a diagnostic? 0 seems to be the regular calculation
  type(physics_state),       target  :: state
  type(physics_buffer_desc), pointer :: pbuf(:)
  integer                            :: nnite          ! number of night columns
  integer                            :: idxnite(pcols) ! local column indices of night columns

  real(r8) :: tau    (pcols,0:pver,nswbands) ! aerosol extinction optical depth
  real(r8) :: tau_w  (pcols,0:pver,nswbands) ! aerosol single scattering albedo * tau
  real(r8) :: tau_w_g(pcols,0:pver,nswbands) ! aerosol assymetry parameter * tau * w
  real(r8) :: tau_w_f(pcols,0:pver,nswbands) ! aerosol forward scattered fraction * tau * w

  real(r8) :: lw_values(nlwbands) ! temporary storage for optics comparisons
  real(r8) :: sw_values(nswbands,4) ! temporary storage for optics comparisons

  real(r8) :: odap_aer(pcols,pver,nlwbands) ! [fraction] absorption optical depth, per layer

  integer, dimension(1) :: comp_id, comp_comm, comp_comm_iam
  character(len=20), dimension(1) :: comp_name
  logical, dimension(1) :: comp_iamin

  integer :: i_column, i_layer, i_band, error_code, compute_comm


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
  aerosol_state => aerosol%new_state( )
  call set_up_optics( )
  call set_raw_mam_states( pbuf, state, raw_mam_state )

  !! Current CAM code run

  call aer_rad_props_init( )
  call aer_rad_props_sw( list_idx, state, pbuf, nnite, idxnite, tau, tau_w, tau_w_g, tau_w_f )
  call aer_rad_props_lw( list_idx, state, pbuf, odap_aer )

  !! MAM run

  do i_column = 1, pcols
    do i_layer = top_lev, pver
      call environmental_state%set_layer_thickness__Pa( state%pdeldry( i_column, i_layer ) )
      call aerosol_state%load_state( raw_mam_state( i_column, i_layer, : ) )
      call aerosol%shortwave_optics( environmental_state, aerosol_state, sw_optics )
      call aerosol%longwave_optics(  environmental_state, aerosol_state, lw_absorption )
      call sw_extinction%get_values(                 sw_values( :, kShortTau ) )
      call sw_single_scatter_albedo%get_values(      sw_values( :, kShortWa  ) )
      call sw_asymmetry_factor%get_values(           sw_values( :, kShortGa  ) )
      call sw_forward_scattered_fraction%get_values( sw_values( :, kShortFa  ) )
      do i_band = 1, nswbands
        call assert( 282393958, &
                     almost_equal( sw_values( i_band, kShortTau ), &
                                   tau( i_column, i_layer, i_band ), &
                                   relative_tolerance = 1.0e-6_musica_dk ) )
        call assert( 444380077, &
                     almost_equal( sw_values( i_band, kShortWa ), &
                                   tau_w( i_column, i_layer, i_band ), &
                                   relative_tolerance = 1.0e-6_musica_dk ) )
        call assert( 274223173, &
                     almost_equal( sw_values( i_band, kShortGa ), &
                                   tau_w_g( i_column, i_layer, i_band ), &
                                   relative_tolerance = 1.0e-6_musica_dk ) )
        call assert( 439115770, &
                     almost_equal( sw_values( i_band, kShortFa ), &
                                   tau_w_f( i_column, i_layer, i_band ), &
                                   relative_tolerance = 1.0e-6_musica_dk ) )
      end do
      call lw_absorption%get_values( lw_values )
      do i_band = 1, nlwbands
        call assert( 331834483, &
                     almost_equal( lw_values( i_band ), &
                                   odap_aer( i_column, i_layer, i_band ), &
                                   relative_tolerance = 1.0e-6_musica_dk ) )
      end do
    end do
  end do

contains

  subroutine set_up_optics( )

    use ai_wavelength_grid,            only : wavelength_grid_t,              &
                                              kWavenumber, kCentimeter
    use musica_property,               only : property_t
    use musica_property_set,           only : property_set_t
    use radconstants,                  only : wavenum_low, wavenum_high,      &
                                              wavenumber1_longwave,           &
                                              wavenumber2_longwave

    character(len=*), parameter :: my_name = "CAM set up optics"
    type(wavelength_grid_t)        :: sw_grid, lw_grid

    class(property_t),    pointer :: property

    sw_grid = wavelength_grid_t( wavenum_low, wavenum_high,                   &
                                 bounds_in = kWavenumber,                     &
                                 base_unit = kCentimeter )
    lw_grid = wavelength_grid_t( wavenumber1_longwave, wavenumber2_longwave,  &
                                 bounds_in = kWavenumber,                     &
                                 base_unit = kCentimeter )
    property => property_t( my_name,                                          &
                            name  = "layer extinction optical depth",         &
                            units = "unitless" )
    sw_extinction => aerosol%new_optics( property, sw_grid )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "layer single-scatter albedo depth",      &
                            units = "unitless" )
    sw_single_scatter_albedo => aerosol%new_optics( property, sw_grid )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "asymmetry factor",                       &
                            units = "unitless" )
    sw_asymmetry_factor => aerosol%new_optics( property, sw_grid )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "forward scattered fraction",             &
                            units = "unitless" )
    sw_forward_scattered_fraction => aerosol%new_optics( property, sw_grid )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "layer absorption optical depth",         &
                            units = "unitless" )
    lw_absorption => aerosol%new_optics( property, lw_grid )
    deallocate( property )

    sw_optics( kShortTau )%ptr_ => sw_extinction
    sw_optics( kShortWa  )%ptr_ => sw_single_scatter_albedo
    sw_optics( kShortGa  )%ptr_ => sw_asymmetry_factor
    sw_optics( kShortFa  )%ptr_ => sw_forward_scattered_fraction

  end subroutine set_up_optics

  subroutine set_raw_mam_states( pbuf, state, raw_states )

    use musica_assert,                 only : assert
    use physics_buffer,                only : pbuf_get_index, pbuf_get_field

    type(physics_buffer_desc), pointer, intent(inout) :: pbuf(:)
    class(physics_state),      target,  intent(in)    :: state
    real(kind=r8),                      intent(inout) :: raw_states(:,:,:)

    integer                :: i_column, i_layer
    integer                :: pbuf_id, errcode
    real(kind=r8), pointer :: dgnumwet_array(:,:,:), qaerwat_array(:,:,:)
    real(kind=r8)          :: dgnumwet(4), qaerwat(4)

    pbuf_id = -1
    pbuf_id = pbuf_get_index('DGNUMWET', errcode)
    call assert( 343719600, errcode .eq. 0 )
    call pbuf_get_field( pbuf, pbuf_id, dgnumwet_array )

    pbuf_id = -1
    pbuf_id = pbuf_get_index('QAERWAT',  errcode)
    call assert( 894782711, errcode .eq. 0 )
    call pbuf_get_field( pbuf, pbuf_id, qaerwat_array )

    do i_column = 1, size( raw_states, 1 )
      do i_layer = 1, size( raw_states, 2 )
      associate( mam_state => raw_states( i_column, i_layer, : ) )
        dgnumwet = dgnumwet_array( i_column, i_layer, : )
        qaerwat  = qaerwat_array( i_column, i_layer, : )
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
      end associate
      end do
    end do

  end subroutine set_raw_mam_states

end program test_mock_cam
