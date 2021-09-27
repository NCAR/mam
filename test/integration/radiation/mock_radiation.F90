! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_radiation module

!> Mock radiation model for tests passing optical properties of aerosols
module test_mock_radiation

  use ai_optics,                       only : optics_t
  use ai_wavelength_grid,              only : wavelength_grid_t
  use musica_constants,                only : musica_dk

  implicit none
  private

  public :: core_t

  ! number of shorwave spectral intervals
  integer, parameter :: nswbands = 14
  integer, parameter :: nbndsw = 14

  ! number of lw bands
  integer, parameter :: nlwbands = 16
  integer, parameter :: nbndlw = 16

  type :: core_t
    private
    integer                    :: number_of_columns_ = 0
    integer                    :: number_of_layers_  = 0
    type(wavelength_grid_t)    :: shortwave_grid_
    type(wavelength_grid_t)    :: longwave_grid_
    class(optics_t), pointer   :: longwave_absorption_             => null ( )
    class(optics_t), pointer   :: shortwave_extinction_            => null( )
    class(optics_t), pointer   :: shortwave_single_scatter_albedo_ => null( )
    class(optics_t), pointer   :: shortwave_asymmetry_factor_      => null( )
    class(optics_t), pointer   :: shortwave_forward_scattered_fraction_       &
                                                                   => null( )
  contains
    procedure :: run
    procedure, private :: output_optics
    final :: finalize
  end type core_t

  interface core_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates an instance of core_t
  function constructor( aerosol, number_of_columns, number_of_layers )        &
      result( new_core )

    use ai_aerosol,                    only : aerosol_t
    use ai_wavelength_grid,            only : kWavenumber, kCentimeter
    use musica_constants,              only : r8 => musica_dk
    use musica_property,               only : property_t

    !> New radiation core
    type(core_t) :: new_core
    !> Aerosol core
    class(aerosol_t), intent(in) :: aerosol
    !> Number of columns
    integer, intent(in) :: number_of_columns
    !> Number of layers
    integer, intent(in) :: number_of_layers

    character(len=*), parameter :: my_name = "mock radiation consttructor"

    real(r8),parameter :: wavenum_low(nbndsw) = & ! in cm^-1
      (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, &
        8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
    real(r8),parameter :: wavenum_high(nbndsw) = & ! in cm^-1
      (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8, &
       12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)

    real(r8), parameter :: wavenumber1_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
      (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, &
         1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8 /)
    real(r8), parameter :: wavenumber2_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
      (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, 1180._r8, &
         1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8, 3250._r8 /)

    class(property_t),    pointer :: property

    new_core%number_of_columns_ = number_of_columns
    new_core%number_of_layers_  = number_of_layers

    new_core%shortwave_grid_ = wavelength_grid_t( wavenum_low,                &
                                                  wavenum_high,               &
                                                  bounds_in    = kWavenumber, &
                                                  base_unit    = kCentimeter )

    new_core%longwave_grid_ = wavelength_grid_t( wavenumber1_longwave,        &
                                                 wavenumber2_longwave,        &
                                                 bounds_in    = kWavenumber,  &
                                                 base_unit    = kCentimeter )

    property => property_t( my_name,                                          &
                            name  = "layer extinction optical depth",         &
                            units = "unitless" )
    new_core%shortwave_extinction_ =>                                         &
        aerosol%new_optics( property, new_core%shortwave_grid_ )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "layer single-scatter albedo depth",      &
                            units = "unitless" )
    new_core%shortwave_single_scatter_albedo_ =>                              &
        aerosol%new_optics( property, new_core%shortwave_grid_ )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "asymmetry factor",                       &
                            units = "unitless" )
    new_core%shortwave_asymmetry_factor_ =>                                   &
        aerosol%new_optics( property, new_core%shortwave_grid_ )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "forward scattered fraction",             &
                            units = "unitless" )
    new_core%shortwave_forward_scattered_fraction_ =>                         &
        aerosol%new_optics( property, new_core%shortwave_grid_ )
    deallocate( property )
    property => property_t( my_name,                                          &
                            name  = "layer absorption optical depth",         &
                            units = "unitless" )
    new_core%longwave_absorption_ =>                                          &
        aerosol%new_optics( property, new_core%shortwave_grid_ )
    deallocate( property )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Run-time radiation calculation
  subroutine run( this, aerosol, aerosol_state, raw_aerosol_states,           &
      environmental_states )

    use ai_aerosol,                    only : aerosol_t
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr

    !> Radiation model
    class(core_t), intent(inout) :: this
    !> Aerosol module
    class(aerosol_t), intent(in) :: aerosol
    !> Aerosol state object
    class(aerosol_state_t), intent(inout) :: aerosol_state
    !> Raw aerosol state data for the model grid (aerosol state, layer, column)
    real(kind=musica_dk), intent(inout) :: raw_aerosol_states(:,:,:)
    !> Environmental states (layer, column)
    class(environmental_state_t), intent(in) :: environmental_states(:,:)

    integer :: i_column, i_layer, state_index
    type(optics_ptr) :: shortwave_optics(4)

    shortwave_optics(1)%ptr_ => this%shortwave_extinction_
    shortwave_optics(2)%ptr_ => this%shortwave_single_scatter_albedo_
    shortwave_optics(3)%ptr_ => this%shortwave_asymmetry_factor_
    shortwave_optics(4)%ptr_ => this%shortwave_forward_scattered_fraction_

    do i_column = 1, this%number_of_columns_
      do i_layer = 1, this%number_of_layers_
      associate( raw_aero_state => raw_aerosol_states( :, i_layer, i_column ),&
                 env_state => environmental_states( i_layer, i_column ) )
        call aerosol_state%load_state( raw_aero_state )
        call aerosol%shortwave_optics( env_state, aerosol_state,              &
                                       shortwave_optics )
        call aerosol%longwave_optics( env_state, aerosol_state,               &
                                      this%longwave_absorption_ )
        call this%output_optics( i_column, i_layer )
        call aerosol_state%dump_state( raw_aero_state )
      end associate
      end do
    end do

  end subroutine run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Output optical properties for select cells
  subroutine output_optics( this, i_column, i_layer )

    use musica_constants,              only : musica_dk

    class(core_t), intent(in) :: this
    integer,       intent(in) :: i_column
    integer,       intent(in) :: i_layer

    real(kind=musica_dk) :: sw_values(nswbands)
    real(kind=musica_dk) :: lw_values(nlwbands)

    if( i_column .eq. 12 .and. i_layer .eq. 1 ) then
      write(*,*) "shortwave wa at layer 1 of column 12"
      call this%shortwave_single_scatter_albedo_%get_values( sw_values )
      write(*,*) sw_values
    end if

    if( i_column .eq. 20 .and. i_layer .eq. 5 ) then
      write(*,*) "longwave absorption at layer 5 of column 20"
      call this%longwave_absorption_%get_values( lw_values )
      write(*,*) lw_values
    end if

  end subroutine output_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the core
  subroutine finalize( this )

    type(core_t), intent(inout) :: this

    if( associated( this%shortwave_extinction_ ) )                            &
      deallocate( this%shortwave_extinction_ )
    if( associated( this%shortwave_single_scatter_albedo_ ) )                 &
      deallocate( this%shortwave_single_scatter_albedo_ )
    if( associated( this%shortwave_asymmetry_factor_ ) )                      &
      deallocate( this%shortwave_asymmetry_factor_ )
    if( associated( this%shortwave_forward_scattered_fraction_ ) )            &
      deallocate( this%shortwave_forward_scattered_fraction_ )
    if( associated( this%longwave_absorption_ ) )                             &
      deallocate( this%longwave_absorption_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module test_mock_radiation
