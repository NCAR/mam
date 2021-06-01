! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_accessor module

!> The optics_accessor_t type and related functions
module mam_optics_accessor

  use ai_accessor,                     only : accessor_t
  use mam_interpolator,                only : interpolator_t

  implicit none
  private

  public :: optics_accessor_t

  !> Optics accessor for MAM
  type, extends(accessor_t) :: optics_accessor_t
    private
    logical :: is_shortwave      = .false.
    logical :: is_longwave       = .false.
    logical :: do_interpolation_ = .false.
    type(interpolator_t) :: interpolator_
    integer :: layer_optical_depth_                       = -1
    integer :: layer_scattering_optical_depth_            = -1
    integer :: layer_asymmetric_scattering_optical_depth_ = -1
    integer :: layer_absorption_optical_depth_            = -1
  contains
    procedure :: calculate
  end type optics_accessor_t

  interface optics_accessor_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an optics accessor
  function constructor( optics ) result( new_obj )

    use ai_constants,                  only : r8 => kDouble
    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use ai_util,                       only : assert_msg, die_msg

    type(optics_accessor_t), pointer    :: new_obj
    class(optics_t),         intent(in) :: optics

    type(wavelength_grid_t) :: shortwave, longwave
    character(len=:), allocatable :: property
    integer :: i_prop

    ! Shortwave and longwave spectral bounds (cm-1)
    real(r8), parameter :: shortwave_lower(14) = &
      (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, &
        8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
    real(r8), parameter :: shortwave_upper(14) = &
      (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8, &
       12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)
    real(r8), parameter :: longwave_lower(16) = &
      (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8,&
         1080._r8, 1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8,&
         2390._r8, 2600._r8 /)
    real(r8), parameter :: longwave_upper(16) = &
      (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8,&
         1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8,&
         2600._r8, 3250._r8 /)

    allocate( new_obj )

    shortwave = wavelength_grid_t( lower_bounds = shortwave_lower,           &
                                   upper_bounds = shortwave_upper,           &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    longwave  = wavelength_grid_t( lower_bounds = longwave_lower,            &
                                   upper_bounds = longwave_upper,            &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    do i_prop = 1, optics%number_of_properties( )
      property = optics%property_name( i_prop )
      if( property .eq. "layer optical depth" ) then
        new_obj%layer_optical_depth_ = i_prop
        new_obj%is_shortwave = .true.
      else if( property .eq. "layer scattering optical depth" ) then
        new_obj%layer_scattering_optical_depth_ = i_prop
        new_obj%is_shortwave = .true.
      else if( property .eq. "layer asymmetric scattering optical depth" ) then
        new_obj%layer_asymmetric_scattering_optical_depth_ = i_prop
        new_obj%is_shortwave = .true.
      else if( property .eq. "layer absorption optical depth" ) then
        new_obj%layer_absorption_optical_depth_ = i_prop
        new_obj%is_longwave = .true.
      else
        call die_msg( 312536255, "Unsupported optical property: "//           &
                      trim( property ) )
      end if
    end do

    call assert_msg( 195405898,                                               &
                     new_obj%is_shortwave .neqv. new_obj%is_longwave,         &
                     "Optical properties on combined shortwave and "//        &
                     "longwave grids not supported." )

    if( new_obj%is_shortwave ) then
      if( optics%grid( ) .ne. shortwave ) then
        new_obj%interpolator_ = interpolator_t( shortwave, optics%grid( ) )
        new_obj%do_interpolation_ = .true.
      end if
    end if

    if( new_obj%is_longwave ) then
      if( optics%grid( ) .ne. shortwave ) then
        new_obj%interpolator_ = interpolator_t( shortwave, optics%grid( ) )
        new_obj%do_interpolation_ = .true.
      end if
    end if

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the optical properties
  subroutine calculate( this, modes, environmental_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use mam_mode,                      only : mode_t

    class(optics_accessor_t),     intent(in)    :: this
    class(mode_t),                intent(in)    :: modes(:)
    class(environmental_state_t), intent(in)    :: environmental_state
    class(optics_t),              intent(inout) :: optics

    ! look up calculation
    optics%values_(:,:,:,:) = 1.0

  end subroutine calculate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_accessor
