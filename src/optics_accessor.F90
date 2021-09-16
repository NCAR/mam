! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_accessor module

!> The optics_accessor_t type and related functions
module mam_optics_accessor

  use ai_accessor,                     only : accessor_t
  use mam_optics_constants
  use musica_interpolator,             only : interpolator_t

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
    integer :: layer_extinction_optical_depth_index_ = -1
    integer :: layer_single_scatter_albedo_index_    = -1
    integer :: asymmetry_factor_index_               = -1
    integer :: forward_scattered_fraction_index_     = -1
    integer :: layer_absorption_optical_depth_index_ = -1
  contains
    procedure :: is_shortwave
    procedure :: is_longwave
    procedure :: get_interpolator
    procedure :: layer_extinction_optical_depth_index
    procedure :: layer_single_scatter_albedo_index
    procedure :: asymmetry_factor_index
    procedure :: forward_scattered_fraction_index
    procedure :: layer_absorption_optical_depth_index
  end type optics_accessor_t

  interface optics_accessor_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an optics accessor
  function constructor( shortwave, longwave, optics ) result( new_obj )

    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use musica_assert,                 only : assert_msg, die_msg
    use musica_constants,              only : r8 => musica_dk
    use musica_interpolator_linear_1D, only : strategy
    use musica_property,               only : property_t
    use musica_property_set,           only : property_set_t
    use musica_string,                 only : string_t

    type(optics_accessor_t),  pointer    :: new_obj
    class(wavelength_grid_t), intent(in) :: shortwave
    class(wavelength_grid_t), intent(in) :: longwave
    class(optics_t),          intent(in) :: optics

    type(string_t) :: property_name
    type(property_set_t), pointer :: property_set
    class(property_t), pointer :: property
    integer :: i_prop

    allocate( new_obj )

    property_set => optics%property_set( )
    do i_prop = 1, property_set%size( )
      property => property_set%get( i_prop )
      property_name = property%name( )
      if( property_name .eq. "layer extinction optical depth" ) then
        new_obj%layer_extinction_optical_depth_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property_name .eq. "layer single-scatter albedo depth" ) then
        new_obj%layer_single_scatter_albedo_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property_name .eq. "asymmetry factor" ) then
        new_obj%asymmetry_factor_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property_name .eq. "forward scattered fraction" ) then
        new_obj%forward_scattered_fraction_index_ = i_prop
        new_obj%is_shortwave_ = .true.
      else if( property_name .eq. "layer absorption optical depth" ) then
        new_obj%layer_absorption_optical_depth_index_ = i_prop
        new_obj%is_longwave_ = .true.
      else
        call die_msg( 312536255, "Unsupported optical property: "//           &
                      trim( property_name%to_char( ) ) )
      end if
      deallocate( property )
    end do
    deallocate( property_set )

    call assert_msg( 195405898,                                               &
                     new_obj%is_shortwave_ .neqv. new_obj%is_longwave_,       &
                     "Optical properties on combined shortwave and "//        &
                     "longwave grids not supported." )

    if( new_obj%is_shortwave_ ) then
      new_obj%interpolator_ =                                                 &
          interpolator_t( strategy, shortwave, optics%grid( ) )
    end if

    if( new_obj%is_longwave_ ) then
      new_obj%interpolator_ =                                                 &
          interpolator_t( strategy, longwave, optics%grid( ) )
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

  !> Returns the index for layer extiction optical depth in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_extinction_optical_depth_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_extinction_optical_depth_index =                                    &
        this%layer_extinction_optical_depth_index_

  end function layer_extinction_optical_depth_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for layer single-scatter albedo in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function layer_single_scatter_albedo_index( this )

    class(optics_accessor_t), intent(in) :: this

    layer_single_scatter_albedo_index =                                       &
        this%layer_single_scatter_albedo_index_

  end function layer_single_scatter_albedo_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for asymmetry factor in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function asymmetry_factor_index( this )

    class(optics_accessor_t), intent(in) :: this

    asymmetry_factor_index = this%asymmetry_factor_index_

  end function asymmetry_factor_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index for forward scattered fraction in optics_t%values_
  !!
  !! Returns -1 if this property is not included in the accessor
  integer function forward_scattered_fraction_index( this )

    class(optics_accessor_t), intent(in) :: this

    forward_scattered_fraction_index = this%forward_scattered_fraction_index_

  end function forward_scattered_fraction_index

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
