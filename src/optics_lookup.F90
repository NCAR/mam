! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_lookup module

!> The optics_lookup_t type and related functions
module mam_optics_lookup

  use musica_constants,                only : musica_dk
  use musica_lookup_2D_axis,           only : lookup_2D_axis_t
  use musica_string,                   only : string_t

  implicit none
  private

  public :: optics_lookup_t

  !> @name Local indices for optics parameters
  !! @{
  integer, parameter :: kAbsorption      = 1
  integer, parameter :: kExtinction      = 2
  integer, parameter :: kAsymmetryFactor = 3
  !> @}
  !> Number of possible optics parameters
  integer, parameter :: kNumberOfParameters = 3

  type :: optics_data_t
    private
    type(string_t) :: parameter_name_
    logical        :: is_loaded_      = .false.
    !> Optics lookup data (Chebychev coefficients, real refractive index,
    !!                     imaginary refractive index, band)
    real(kind=musica_dk), allocatable :: values_(:,:,:,:)
  contains
    procedure :: lookup => optics_data_lookup
  end type optics_data_t

  interface optics_data_t
    procedure :: optics_data_constructor
  end interface optics_data_t

  type :: optics_lookup_t
    private
    !> Optics parameter lookup data
    type(optics_data_t) :: data_( kNumberOfParameters )
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
  end type optics_lookup_t

  interface optics_lookup_t
    procedure :: constructor
  end interface optics_lookup_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of optics lookup objects
  function constructor( config ) result( new_lookup )

    use musica_assert,                 only : assert, assert_msg
    use musica_config,                 only : config_t
    use musica_file_util,              only : get_file_data
    use musica_lookup_axis,            only : lookup_axis_t

    type(optics_lookup_t)                :: new_lookup
    class(config_t),       intent(inout) :: config

    character(len=*), parameter :: my_name = "optics_lookup_t constructor"
    type(string_t) :: file_path, variable_name, axis_1_name, axis_2_name
    type(lookup_axis_t) :: axis_1, axis_2
    real(kind=musica_dk), allocatable :: axis_1_values(:,:), axis_2_values(:,:)
    integer :: i_band, i_param
    logical :: found

    call config%get( "file path", file_path, my_name )
    call config%get( "axis 1", axis_1_name, my_name )
    call get_file_data( file_path, axis_1_name, axis_1_values, my_name )
    call config%get( "axis 2", axis_2_name, my_name )
    call get_file_data( file_path, axis_2_name, axis_2_values, my_name )
    call assert_msg( 756675500, size( axis_1_values, 2 ) .eq.                 &
                                size( axis_2_values, 2 ),                     &
                     "Optics lookup axis dimension mismatch" )
    new_lookup%number_of_wavelength_bands_ = size( axis_1_values, 2 )
    allocate( new_lookup%axes_( new_lookup%number_of_wavelength_bands_ ) )
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
    new_lookup%data_( kAbsorption      ) =                                    &
        optics_data_t( "specific absorption lookup", file_path, config )
    new_lookup%data_( kExtinction      ) =                                    &
        optics_data_t( "specific extinction lookup", file_path, config )
    new_lookup%data_( kAsymmetryFactor ) =                                    &
        optics_data_t( "asymmetry factor lookup",    file_path, config )
    do i_param = 1, kNumberOfParameters
      if( .not. new_lookup%data_( i_param )%is_loaded_ ) cycle
      call assert_msg( 284936197,                                             &
                       size( new_lookup%data_( i_param )%values_, 4 )         &
                       .eq. new_lookup%number_of_wavelength_bands_,           &
                       "Dimension mismatch in optics_lookup_t data" )
    end do

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Lookup optics properties for a given wavelength-dependent refractive
  !! index
  !!
  !! \todo there is no check to make sure the requested data is actually
  !!       present, as this would require making this not a pure function.
  !!       Check to see if this being a pure function has any benefit
  pure subroutine get_optics( this, refractive_indices, absorption,           &
      extinction, asymmetry_factor )

    class(optics_lookup_t),         intent(in)  :: this
    !> Complex indices of refraction (wavelength band)
    complex(kind=musica_dk),        intent(in)  :: refractive_indices(:)
    !> @name optical properties (Chebychev coefficient, wavelength band)
    !! {
    real(kind=musica_dk), optional, intent(out) :: absorption(:,:)
    real(kind=musica_dk), optional, intent(out) :: extinction(:,:)
    real(kind=musica_dk), optional, intent(out) :: asymmetry_factor(:,:)
    !> @}

    integer              :: indices(2), i_band
    real(kind=musica_dk) :: residuals(2)

    do i_band = 1, this%number_of_wavelength_bands_
    associate( axis => this%axes_( i_band ) )
      indices(:) = 1
      call axis%find( (/real( refractive_indices( i_band ) ),                 &
                        abs( aimag( refractive_indices( i_band ) ) )/),       &
                      indices, residuals )
      if( present( absorption ) ) then
        call this%data_( kAbsorption )%lookup( i_band, indices, residuals,    &
                                               axis, absorption( :, i_band ) )
      end if
      if( present( extinction ) ) then
        call this%data_( kExtinction )%lookup( i_band, indices, residuals,    &
                                               axis, extinction( :, i_band ) )
      end if
      if( present( asymmetry_factor ) ) then
        call this%data_( kAsymmetryFactor )%lookup( i_band, indices,          &
                                               residuals, axis,               &
                                               asymmetry_factor( :, i_band ) )
      end if
    end associate
    end do

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Normalizes a radius to the range used to calculate the Chebychev
  !! coefficients
  !!
  !! Radii are converted to ln(radius) prior to normalization
  !!
  !! Normalized radii range from -1...1
  real(kind=musica_dk) elemental function normalize_radius( this, radius__m )

    class(optics_lookup_t), intent(in) :: this
    !> Aerosol surface mode radius
    !! (mode radius of the surface area distribution)
    real(kind=musica_dk),   intent(in) :: radius__m

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

    class(optics_lookup_t), intent(in) :: this

    maximum_radius__m = exp( this%maximum_ln_radius_ )

  end function maximum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the minimum radius of the range used to calculate the Chebychev
  !! coefficients
  real(kind=musica_dk) elemental function minimum_radius__m( this )

    class(optics_lookup_t), intent(in) :: this

    minimum_radius__m = exp( this%minimum_ln_radius_ )

  end function minimum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Private functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of optics lookup data objects
  function optics_data_constructor( parameter_name, file_path, config )       &
      result( new_data )

    use musica_config,                 only : config_t
    use musica_file_util,              only : get_file_data

    type(optics_data_t)               :: new_data
    character(len=*),   intent(in)    :: parameter_name
    type(string_t),     intent(in)    :: file_path
    class(config_t),    intent(inout) :: config

    character(len=*), parameter :: my_name = "optics table loader"
    type(config_t) :: table_config
    type(string_t) :: variable_name
    logical        :: found

    new_data%parameter_name_ = parameter_name
    call config%get( parameter_name, table_config, my_name, found = found )
    if( .not. found ) then
      allocate( new_data%values_(0,0,0,0) )
      return
    end if
    call table_config%get( "variable name", variable_name, my_name )
    call get_file_data( file_path, variable_name, new_data%values_, my_name )
    new_data%is_loaded_ = .true.

  end function optics_data_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Looks up optical property data at a given wavelength band
  pure subroutine optics_data_lookup( this, band_index, indices, residuals,   &
      axis, chebychev_values )

    class(optics_data_t),   intent(in)  :: this
    integer,                intent(in)  :: band_index
    integer,                intent(in)  :: indices(2)
    real(kind=musica_dk),   intent(in)  :: residuals(2)
    type(lookup_2D_axis_t), intent(in)  :: axis
    real(kind=musica_dk),   intent(out) :: chebychev_values(:)

    call axis%interpolate( this%values_( :,                                   &
                                         indices(1):indices(1)+1,             &
                                         indices(2):indices(2)+1,             &
                                         band_index ),                        &
                           residuals, chebychev_values )

  end subroutine optics_data_lookup

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_lookup
