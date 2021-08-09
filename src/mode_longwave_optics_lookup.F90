! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_mode_longwave_optics_lookup module

!> The mode_longwave_optics_lookup_t type and related functions
module mam_mode_longwave_optics_lookup

  use musica_constants,                only : musica_dk
  use musica_lookup_2D_axis,           only : lookup_2D_axis_t

  implicit none
  private

  public :: mode_longwave_optics_lookup_t

  type :: mode_longwave_optics_lookup_t
    private
    !> @name Optics lookup data (Chebychev coefficients, real refractive index,
    !!                           maginary refractive index, band)
    !! {
    real(kind=musica_dk), allocatable :: absorption_(:,:,:,:)
    !> @}
    integer                             :: number_of_wavelength_bands_ = 0
    real(kind=musica_dk)                :: minimum_ln_radius_ = 0.0
    real(kind=musica_dk)                :: maximum_ln_radius_ = 0.0
    type(lookup_2D_axis_t), allocatable :: axes_(:)
  contains
    procedure :: get_optics
    procedure :: normalize_radius
    procedure :: maximum_radius__m
    procedure :: minimum_radius__m
  end type mode_longwave_optics_lookup_t

  interface mode_longwave_optics_lookup_t
    procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of mode optics lookup objects
  function constructor( config ) result( new_lookup )

    use musica_assert,                 only : assert, assert_msg
    use musica_config,                 only : config_t
    use musica_file_util,              only : get_file_data
    use musica_lookup_axis,            only : lookup_axis_t
    use musica_string,                 only : string_t

    type(mode_longwave_optics_lookup_t)                :: new_lookup
    class(config_t),                     intent(inout) :: config

    character(len=*), parameter :: my_name =                                  &
        "mode_longwave_optics_lookup_t constructor"
    type(config_t) :: table_config
    type(string_t) :: file_path, variable_name, axis_1_name, axis_2_name
    type(lookup_axis_t) :: axis_1, axis_2
    real(kind=musica_dk), allocatable :: axis_1_values(:,:), axis_2_values(:,:)
    integer :: i_band

    call config%get( "file path", file_path, my_name )
    call config%get( "specific absorption lookup", table_config, my_name )
    call table_config%get( "variable name", variable_name, my_name )
    call get_file_data( file_path, variable_name, new_lookup%absorption_,     &
                        my_name )
    new_lookup%number_of_wavelength_bands_ = size( new_lookup%absorption_, 4 )
    call config%get( "axis 1", axis_1_name, my_name )
    call get_file_data( file_path, axis_1_name, axis_1_values, my_name )
    call config%get( "axis 2", axis_2_name, my_name )
    call get_file_data( file_path, axis_2_name, axis_2_values, my_name )
    allocate( new_lookup%axes_( new_lookup%number_of_wavelength_bands_ ) )
    call assert( 485755406, size( axis_1_values, 2 ) .eq.                     &
                            new_lookup%number_of_wavelength_bands_ )
    call assert( 598073751, size( axis_2_values, 2 ) .eq.                     &
                            new_lookup%number_of_wavelength_bands_ )
    do i_band = 1, new_lookup%number_of_wavelength_bands_
      axis_1 = lookup_axis_t( axis_1_name, axis_1_values( :, i_band ) )
      axis_2 = lookup_axis_t( axis_2_name, axis_2_values( :, i_band ) )
      new_lookup%axes_( i_band ) = lookup_2D_axis_t( axis_1, axis_2 )
    end do
    variable_name = "minimum_radius"
    call get_file_data( file_path, variable_name,                             &
                        new_lookup%minimum_ln_radius_, my_name )
    new_lookup%minimum_ln_radius_ = log( new_lookup%minimum_ln_radius_ )
    variable_name = "maximum_radius"
    call get_file_data( file_path, variable_name,                             &
                        new_lookup%maximum_ln_radius_, my_name )
    new_lookup%maximum_ln_radius_ = log( new_lookup%maximum_ln_radius_ )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Lookup optics properties for given refractive indices
  pure subroutine get_optics( this, refractive_indices, absorption )

    class(mode_longwave_optics_lookup_t), intent(in)  :: this
    !> Complex indices of refraction (wavelength band)
    complex(kind=musica_dk),              intent(in)  :: refractive_indices(:)
    !> @name optical properties (Chebychev coefficient, wavelength band)
    !! {
    real(kind=musica_dk), optional,       intent(out) :: absorption(:,:)
    !> @}

    integer              :: indices(2), i_band
    real(kind=musica_dk) :: residuals(2)

    do i_band = 1, this%number_of_wavelength_bands_
      indices(:) = 1
      call this%axes_( i_band )%find( (/real( refractive_indices( i_band ) ), &
                              abs( aimag( refractive_indices( i_band ) ) )/), &
                            indices, residuals )
      if( present( absorption ) ) then
        call this%axes_( i_band )%interpolate(                                &
            this%absorption_( :,                                              &
                              indices(1):indices(1)+1,                        &
                              indices(2):indices(2)+1,                        &
                              i_band ),                                       &
            residuals, absorption(:, i_band ) )
      end if
    end do

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Normalizes a radius to the range used to calculate the Chebychev
  !! coefficients
  !!
  !! Radii are converted to ln(radius) prior to normalization
  real(kind=musica_dk) elemental function normalize_radius( this, radius__m )

    class(mode_longwave_optics_lookup_t), intent(in) :: this
    !> Aerosol surface mode radius
    !! (mode radius of the surface area distribution)
    real(kind=musica_dk),                  intent(in) :: radius__m

    associate( rmin => this%minimum_ln_radius_,                               &
               rmax => this%maximum_ln_radius_ )
      normalize_radius = log( radius__m )
      normalize_radius = max( normalize_radius, rmin )
      normalize_radius = min( normalize_radius, rmax )
      normalize_radius = ( 2.0_musica_dk * normalize_radius - rmax - rmin ) / &
                         ( rmax - rmin )
    end associate

  end function normalize_radius

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the maximum radius of the range used to calculate the Chebychev
  !! coefficients
  real(kind=musica_dk) elemental function maximum_radius__m( this )

    class(mode_longwave_optics_lookup_t), intent(in) :: this

    maximum_radius__m = exp( this%maximum_ln_radius_ )

  end function maximum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the minimum radius of the range used to calculate the Chebychev
  !! coefficients
  real(kind=musica_dk) elemental function minimum_radius__m( this )

    class(mode_longwave_optics_lookup_t), intent(in) :: this

    minimum_radius__m = exp( this%minimum_ln_radius_ )

  end function minimum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_mode_longwave_optics_lookup
