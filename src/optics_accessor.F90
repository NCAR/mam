! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_accessor module

!> The optics_accessor_t type and related functions
module mam_optics_accessor

  use ai_accessor,                     only : accessor_t
  use mam_interpolator,                only : interpolator_t
  use mam_optics_constants

  implicit none
  private

  public :: optics_accessor_t

  !> Optics accessor for MAM
  type, extends(accessor_t) :: optics_accessor_t
    private
    logical :: is_shortwave_     = .false.
    logical :: is_longwave_      = .false.
    logical :: do_interpolation_ = .false.
    type(interpolator_t) :: interpolator_
    integer :: layer_optical_depth_index_                       = -1
    integer :: layer_scattering_optical_depth_index_            = -1
    integer :: layer_asymmetric_scattering_optical_depth_index_ = -1
    integer :: layer_absorption_optical_depth_index_            = -1
  contains
    procedure :: is_shortwave
    procedure :: is_longwave
    procedure :: get_interpolator
    procedure :: layer_optical_depth_index
    procedure :: layer_scattering_optical_depth_index
    procedure :: layer_asymmetric_scattering_optical_depth_index
    procedure :: layer_absorption_optical_depth_index
  end type optics_accessor_t

  interface optics_accessor_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an optics accessor
  function constructor( shortwave, longwave, optics ) result( new_obj )

    use ai_constants,                  only : r8 => kDouble
    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use ai_util,                       only : assert_msg, die_msg

    type(optics_accessor_t),  pointer    :: new_obj
    class(wavelength_grid_t), intent(in) :: shortwave
    class(wavelength_grid_t), intent(in) :: longwave
    class(optics_t),          intent(in) :: optics

    character(len=:), allocatable :: property
    integer :: i_prop

    allocate( new_obj )

    do i_prop = 1, optics%number_of_properties( )
      property = optics%property_name( i_prop )
      if( property .eq. "layer optical depth" ) then
        new_obj%layer_optical_depth_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property .eq. "layer scattering optical depth" ) then
        new_obj%layer_scattering_optical_depth_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property .eq. "layer asymmetric scattering optical depth" ) then
        new_obj%layer_asymmetric_scattering_optical_depth_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property .eq. "layer absorption optical depth" ) then
        new_obj%layer_absorption_optical_depth_index_ = i_prop
        new_obj%is_longwave_ = .true.
      else
        call die_msg( 312536255, "Unsupported optical property: "//           &
                      trim( property ) )
      end if
    end do

    call assert_msg( 195405898,                                               &
                     new_obj%is_shortwave_ .neqv. new_obj%is_longwave_,       &
                     "Optical properties on combined shortwave and "//        &
                     "longwave grids not supported." )

    if( new_obj%is_shortwave_ ) then
      new_obj%interpolator_ = interpolator_t( shortwave, optics%grid( ) )
    end if

    if( new_obj%is_longwave_ ) then
      new_obj%interpolator_ = interpolator_t( longwave, optics%grid( ) )
    end if

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether the accessor is for shortwave optics
  logical function is_shortwave( this )

    class(optics_accessor_t), intent(in) :: this

    is_shortwave = this%is_shortwave_

  end function is_shortwave

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether the accessor is for longwave optics
  logical function is_longwave( this )

    class(optics_accessor_t), intent(in) :: this

    is_longwave = this%is_longwave_

  end function is_longwave

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the interpolator associated with the accessor
  type(interpolator_t) function get_interpolator( this )

    class(optics_accessor_t), intent(in) :: this

    get_interpolator = this%interpolator_

  end function get_interpolator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for layer optical depth in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_optical_depth_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_optical_depth_index = this%layer_optical_depth_index_

  end function layer_optical_depth_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for layer scattering optical depth in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_scattering_optical_depth_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_scattering_optical_depth_index =                                    &
        this%layer_scattering_optical_depth_index_

  end function layer_scattering_optical_depth_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for layer asymmetric scattering optical depth in
  !! optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_asymmetric_scattering_optical_depth_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_asymmetric_scattering_optical_depth_index =                         &
        this%layer_asymmetric_scattering_optical_depth_index_

  end function layer_asymmetric_scattering_optical_depth_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for layer absorption optical depth in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_absorption_optical_depth_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_absorption_optical_depth_index =                                    &
        this%layer_absorption_optical_depth_index_

  end function layer_absorption_optical_depth_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_accessor
