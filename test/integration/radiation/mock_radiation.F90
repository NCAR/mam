! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_mock_radiation module

!> Mock radiation model for tests passing optical properties of aerosols
module test_mock_radiation

  use ai_accessor,                     only : accessor_t
  use ai_constants,                    only : kDouble
  use ai_optics,                       only : optics_t
  use ai_wavelength_grid,              only : wavelength_grid_t

  implicit none
  private

  public :: core_t

  type :: core_t
    private
    integer                    :: number_of_columns_ = 0
    integer                    :: number_of_layers_  = 0
    type(wavelength_grid_t)    :: shortwave_grid_
    type(wavelength_grid_t)    :: longwave_grid_
    class(accessor_t), pointer :: shortwave_optics_accessor_
    class(accessor_t), pointer :: longwave_optics_accessor_
    type(optics_t)             :: shortwave_optics_
    type(optics_t)             :: longwave_optics_
    !> @name Optical property values (wavelength, property, layer, column)
    !! @{
    real(kind=kDouble), allocatable :: shortwave_optics_values_(:,:,:,:)
    real(kind=kDouble), allocatable :: longwave_optics_values_( :,:,:,:)
    !! @}
  contains
    procedure :: run
  end type core_t

  interface core_t
    module procedure :: constructor
  end interface

  integer, parameter :: kShortTau = 1
  integer, parameter :: kShortWa  = 2
  integer, parameter :: kShortGa  = 3
  integer, parameter :: kShortFa  = 4
  integer, parameter :: kLongAbs  = 1

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates an instance of core_t
  function constructor( aerosol, number_of_columns, number_of_layers )        &
      result( new_core )

    use ai_aerosol,                    only : aerosol_t
    use ai_constants,                  only : r8 => kDouble
    use ai_property,                   only : property_t
    use ai_wavelength_grid,            only : kWavenumber, kCentimeter

    !> New radiation core
    type(core_t) :: new_core
    !> Aerosol core
    class(aerosol_t), intent(in) :: aerosol
    !> Number of columns
    integer, intent(in) :: number_of_columns
    !> Number of layers
    integer, intent(in) :: number_of_layers

    ! number of shorwave spectral intervals
    integer, parameter :: nswbands = 14
    integer, parameter :: nbndsw = 14

    real(r8),parameter :: wavenum_low(nbndsw) = & ! in cm^-1
      (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, &
        8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
    real(r8),parameter :: wavenum_high(nbndsw) = & ! in cm^-1
      (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8, &
       12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)

    ! number of lw bands
    integer, parameter :: nlwbands = 16
    integer, parameter :: nbndlw = 16

    real(r8), parameter :: wavenumber1_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
      (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, &
         1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8 /)
    real(r8), parameter :: wavenumber2_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
      (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, 1180._r8, &
         1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8, 3250._r8 /)

    type(property_t), dimension(4) :: shortwave_props
    type(property_t), dimension(1) :: longwave_props

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

    shortwave_props(kShortTau)   =                                            &
        property_t( "layer extinction optical depth",    "unitless" )
    shortwave_props(kShortWa)  =                                              &
        property_t( "layer single-scatter albedo depth", "unitless" )
    shortwave_props(kShortGa) =                                               &
        property_t( "asymmetry factor",                  "unitless" )
    shortwave_props(kShortFa) =                                               &
        property_t( "forward scattered fraction",        "unitless" )
    longwave_props(kLongAbs)     =                                            &
        property_t( "layer absorption optical depth",    "unitless" )

    new_core%shortwave_optics_ = optics_t( shortwave_props,                   &
                                           new_core%shortwave_grid_ )
    new_core%longwave_optics_  = optics_t( longwave_props,                    &
                                           new_core%longwave_grid_  )

    new_core%shortwave_optics_accessor_ =>                                    &
        aerosol%optics_accessor( new_core%shortwave_optics_ )
    new_core%longwave_optics_accessor_  =>                                    &
        aerosol%optics_accessor( new_core%longwave_optics_  )

    allocate( new_core%shortwave_optics_values_( nbndsw,                      &
                                                 size( shortwave_props ),     &
                                                 number_of_layers,            &
                                                 number_of_columns ) )

    allocate( new_core%longwave_optics_values_(  nlwbands,                    &
                                                 size( longwave_props ),      &
                                                 number_of_layers,            &
                                                 number_of_columns ) )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Run-time radiation calculation
  subroutine run( this, aerosol, aerosol_state, raw_aerosol_states,           &
      environmental_states )

    use ai_aerosol,                    only : aerosol_t
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t

    !> Radiation model
    class(core_t), intent(inout) :: this
    !> Aerosol module
    class(aerosol_t), intent(in) :: aerosol
    !> Aerosol state object
    class(aerosol_state_t), intent(inout) :: aerosol_state
    !> Raw aerosol state data for the model grid (aerosol state, layer, column)
    real(kind=kDouble), intent(inout) :: raw_aerosol_states(:,:,:)
    !> Environmental states (layer, column)
    class(environmental_state_t), intent(in) :: environmental_states(:,:)

    integer :: i_column, i_layer, state_index

    do i_column = 1, this%number_of_columns_
      do i_layer = 1, this%number_of_layers_
      associate( raw_aero_state => raw_aerosol_states( :, i_layer, i_column ),&
                 env_state => environmental_states( i_layer, i_column ) )
        call aerosol_state%load_state( raw_aero_state )
        call aerosol%get_optics( this%shortwave_optics_accessor_, env_state,  &
                                 aerosol_state, this%shortwave_optics_ )
        this%shortwave_optics_values_( :, :, i_layer, i_column ) =            &
            this%shortwave_optics_%values_(:,:)
        call aerosol%get_optics( this%longwave_optics_accessor_, env_state,   &
                                 aerosol_state, this%longwave_optics_ )
        this%longwave_optics_values_( :, :, i_layer, i_column ) =             &
            this%longwave_optics_%values_(:,:)
        call aerosol_state%dump_state( raw_aero_state )
      end associate
      end do
    end do

    write(*,*) "shortwave wa at layer 1 of column 12"
    write(*,*) this%shortwave_optics_values_(:, kShortWa, 1, 12 )

    write(*,*) "longwave absorption at layer 5 of column 20"
    write(*,*) this%longwave_optics_values_(:, kLongAbs, 5, 20 )

  end subroutine run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module test_mock_radiation
