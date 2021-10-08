! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_lookup module

!> The optics_lookup_t type and related functions
module mam_optics_lookup

  use ai_wavelength_grid,              only : wavelength_grid_t
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
    type(optics_data_t)     :: data_( kNumberOfParameters )
    integer                 :: number_of_chebyshev_coefficients_ = 0
    type(wavelength_grid_t) :: grid_
    real(kind=musica_dk)    :: minimum_ln_radius_ = 0.0
    real(kind=musica_dk)    :: maximum_ln_radius_ = 0.0
    type(lookup_2D_axis_t), allocatable :: axes_(:)
  contains
    procedure :: get_optics
    procedure :: normalize_radius
    procedure :: maximum_radius__m
    procedure :: minimum_radius__m
    procedure :: number_of_chebyshev_coefficients
    procedure :: number_of_wavelength_bands
    procedure :: grid
    procedure, private :: set_grid
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
    use musica_lookup_axis,            only : lookup_axis_t
    use musica_io,                     only : io_t
    use musica_io_netcdf,              only : io_netcdf_t

    type(optics_lookup_t)                :: new_lookup
    class(config_t),       intent(inout) :: config

    character(len=*), parameter :: my_name = "optics_lookup_t constructor"
    class(io_t), pointer :: lookup_file
    type(config_t) :: grid_config
    type(string_t) :: file_path, variable_name, axis_real_name, axis_imag_name
    type(lookup_axis_t) :: axis_real, axis_imag
    real(kind=musica_dk), allocatable :: axis_real_values(:,:), axis_imag_values(:,:)
    integer :: i_band, i_param
    logical :: found

    call config%get( "file path", file_path, my_name )
    lookup_file => io_netcdf_t( file_path )
    call config%get( "grid", grid_config, my_name )
    call new_lookup%set_grid( grid_config, lookup_file )
    call config%get( "real axis", axis_real_name, my_name )
    call lookup_file%read( axis_real_name, axis_real_values, my_name )
    call config%get( "imaginary axis", axis_imag_name, my_name )
    call lookup_file%read( axis_imag_name, axis_imag_values, my_name )
    call assert_msg( 756675500, size( axis_real_values, 2 ) .eq.                 &
                                size( axis_imag_values, 2 ),                     &
                     "Optics lookup axis dimension mismatch" )
    allocate( new_lookup%axes_( new_lookup%grid_%number_of_sections( ) ) )
    do i_band = 1, new_lookup%grid_%number_of_sections( )
      axis_real = lookup_axis_t( axis_real_name, axis_real_values( :, i_band ) )
      axis_imag = lookup_axis_t( axis_imag_name, axis_imag_values( :, i_band ) )
      new_lookup%axes_( i_band ) = lookup_2D_axis_t( axis_real, axis_imag )
    end do
    variable_name = "minimum_radius"
    call lookup_file%read( variable_name,                                     &
                           new_lookup%minimum_ln_radius_, my_name )
    new_lookup%minimum_ln_radius_ = log( new_lookup%minimum_ln_radius_ )
    variable_name = "maximum_radius"
    call lookup_file%read( variable_name,                                     &
                           new_lookup%maximum_ln_radius_, my_name )
    new_lookup%maximum_ln_radius_ = log( new_lookup%maximum_ln_radius_ )
    new_lookup%data_( kAbsorption      ) =                                    &
        optics_data_t( "specific absorption lookup", lookup_file, config )
    new_lookup%data_( kExtinction      ) =                                    &
        optics_data_t( "specific extinction lookup", lookup_file, config )
    new_lookup%data_( kAsymmetryFactor ) =                                    &
        optics_data_t( "asymmetry factor lookup",    lookup_file, config )
    do i_param = 1, kNumberOfParameters
      if( .not. new_lookup%data_( i_param )%is_loaded_ ) cycle
      if( new_lookup%number_of_chebyshev_coefficients_ .eq. 0 ) then
        new_lookup%number_of_chebyshev_coefficients_ =                        &
          size( new_lookup%data_( i_param )%values_, 1 )
      else
        call assert_msg( 168034110,                                           &
                         size( new_lookup%data_( i_param )%values_, 1 )       &
                         .eq. new_lookup%number_of_chebyshev_coefficients_,   &
                         "Dimension mismatch in optics_lookup_t data" )
      end if
      call assert_msg( 531859329,                                             &
                       size( new_lookup%data_( i_param )%values_, 2 )         &
                       .eq. axis_real%size( ),                                &
                       "Dimension mismatch in optics_lookup_t data" )
      call assert_msg( 521820197,                                             &
                       size( new_lookup%data_( i_param )%values_, 3 )         &
                       .eq. axis_imag%size( ),                                &
                       "Dimension mismatch in optics_lookup_t data" )
      call assert_msg( 284936197,                                             &
                       size( new_lookup%data_( i_param )%values_, 4 )         &
                       .eq. new_lookup%grid_%number_of_sections( ),           &
                       "Dimension mismatch in optics_lookup_t data" )
    end do
    deallocate( lookup_file )

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

    do i_band = 1, this%grid_%number_of_sections( )
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

  !> Returns the number of Chebyshev coefficients per band and property
  integer elemental function number_of_chebyshev_coefficients( this )

    class(optics_lookup_t), intent(in) :: this

    number_of_chebyshev_coefficients = this%number_of_chebyshev_coefficients_

  end function number_of_chebyshev_coefficients

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of wavelength bands for each property
  integer elemental function number_of_wavelength_bands( this )

    class(optics_lookup_t), intent(in) :: this

    number_of_wavelength_bands = this%grid_%number_of_sections( )

  end function number_of_wavelength_bands

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the wavelength grid that the properties are returned on
  type(wavelength_grid_t) function grid( this )

    class(optics_lookup_t), intent(in) :: this

    grid = this%grid_

  end function grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Private functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the wavelength grid for an optics_lookup_t object
  subroutine set_grid( this, config, lookup_file )

    use ai_wavelength_grid,            only : kCentimeter, kWavenumber
    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_io,                     only : io_t

    class(optics_lookup_t), intent(inout) :: this
    class(config_t),        intent(inout) :: config
    class(io_t),            intent(inout) :: lookup_file

    character(len=*), parameter :: my_name = "optics_lookup_t grid loader"
    type(string_t) :: var_name, lb_units, ub_units
    real(kind=musica_dk), allocatable :: lower_bounds(:), upper_bounds(:)

    call config%get( "lower bounds", var_name, my_name )
    call lookup_file%read( var_name, lower_bounds, my_name )
    lb_units = lookup_file%variable_units( var_name, my_name )
    call config%get( "upper bounds", var_name, my_name )
    call lookup_file%read( var_name, upper_bounds, my_name )
    ub_units = lookup_file%variable_units( var_name, my_name )
    call assert_msg( 950480637, lb_units .eq. ub_units,                       &
                     "Units mismatch for upper and lower wavelength "//       &
                     "bounds" )
    call assert_msg( 709050082, lb_units .eq. "cm-1",                         &
                     "Expected wavlength bounds in units of 'cm-1'" )
    this%grid_ = wavelength_grid_t( lower_bounds = lower_bounds,              &
                                    upper_bounds = upper_bounds,              &
                                    bounds_in    = kWavenumber,               &
                                    base_unit    = kCentimeter )

  end subroutine set_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of optics lookup data objects
  function optics_data_constructor( parameter_name, lookup_file, config )     &
      result( new_data )

    use musica_config,                 only : config_t
    use musica_io,                     only : io_t

    type(optics_data_t)               :: new_data
    character(len=*),   intent(in)    :: parameter_name
    class(io_t),        intent(inout) :: lookup_file
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
    call lookup_file%read( variable_name, new_data%values_, my_name )
    new_data%is_loaded_ = .true.

  end function optics_data_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Looks up optical property data at a given wavelength band
  pure subroutine optics_data_lookup( this, band_index, indices, residuals,   &
      axis, chebyshev_values )

    class(optics_data_t),   intent(in)  :: this
    integer,                intent(in)  :: band_index
    integer,                intent(in)  :: indices(2)
    real(kind=musica_dk),   intent(in)  :: residuals(2)
    type(lookup_2D_axis_t), intent(in)  :: axis
    real(kind=musica_dk),   intent(out) :: chebyshev_values(:)

    call axis%interpolate( this%values_( :,                                   &
                                         indices(1):indices(1)+1,             &
                                         indices(2):indices(2)+1,             &
                                         band_index ),                        &
                           residuals, chebyshev_values )

  end subroutine optics_data_lookup

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_lookup
