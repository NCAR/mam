! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_mode module

!> The mode_t type and related functions
module mam_mode

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use mam_mode_longwave_optics_lookup, only : mode_longwave_optics_lookup_t
  use mam_mode_shortwave_optics_lookup,only : mode_shortwave_optics_lookup_t
  use mam_species,                     only : species_t
  use musica_constants,                only : musica_dk

  implicit none
  private

  public :: mode_t, mode_state_t

  real(kind=musica_dk), parameter :: kMinRadiusM = 0.01e-6_musica_dk
  real(kind=musica_dk), parameter :: kMaxRadiusM = 25.0e-6_musica_dk

  !> An aerosol mode
  type, extends(aerosol_t) :: mode_t
    private
    !> Geometric mean diameter [m] of number distribution
    real(kind=musica_dk)           :: geometric_mean_diameter__m_
    !> Geometric standard deviation of number distribution
    real(kind=musica_dk)           :: geometric_standard_deviation_
    !> Chemical species that can be present in the mode
    type(species_t), allocatable :: species_(:)
    !> Shortwave optical property lookup table
    type(mode_shortwave_optics_lookup_t) :: shortwave_lookup_
    !> Longwave optical property lookup table
    type(mode_longwave_optics_lookup_t)  :: longwave_lookup_
  contains
    procedure :: get_new_state
    procedure :: optics_accessor
    procedure :: get_optics
    procedure :: print_state
    procedure :: calculate_shortwave_optics
    procedure :: calculate_longwave_optics
    procedure :: geometric_mean_diameter_of_number_distribution__m
    procedure :: geometric_standard_deviation_of_number_distribution
    procedure :: wet_volume_to_mass_mixing_ratio__m3_kg
    procedure :: wet_number_mode_radius__m
    procedure :: wet_number_mode_diameter__m
    procedure :: wet_surface_mode_radius__m
    procedure :: wet_surface_mode_diameter__m
    procedure :: number_mixing_ratio__num_mol
    procedure, private :: specific_absorption__m2_kg
    procedure, private :: specific_extinction__m2_kg
    procedure, private :: asymmetry_parameter
  end type mode_t

  interface mode_t
    module procedure :: constructor
  end interface mode_t

  !> Aerosol mode state
  type, extends(aerosol_state_t) :: mode_state_t
    private
    integer                     :: number_of_species_
    !> Mode diameter of the wet aerosol number distribution [m]
    real(kind=musica_dk), pointer :: wet_number_mode_diameter__m_ => null( )
    !> Particle number mixing ratio per mol_air [# mol-1]
    !! \todo Is this dry air or wet air?
    real(kind=musica_dk), pointer :: number_mixing_ratio__num_mol_ => null( )
    !> Mass mixing ratio of each mode species relative to dry air [kg kg-1]
    real(kind=musica_dk), pointer :: mass_mixing_ratio__kg_kg_(:) => null( )
  contains
    procedure :: raw_size
    procedure :: load_state
    procedure :: dump_state
    procedure :: randomize
  end type mode_state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of mode_t objects
  function constructor( config ) result( new_obj )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t

    type(mode_t)                   :: new_obj
    class(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "MAM mode_t constructor"
    type(config_t) :: spectrum, species_set, species
    class(iterator_t), pointer :: iter
    integer :: i_species

    call config%get( "geometric mean diameter of number distribution", "m",   &
                     new_obj%geometric_mean_diameter__m_,   my_name )
    call config%get( "geometric standard deviation of number distribution",   &
                     new_obj%geometric_standard_deviation_, my_name )
    call config%get( "species", species_set, my_name )
    allocate( new_obj%species_( species_set%number_of_children( ) ) )
    iter => species_set%get_iterator( )
    i_species = 1
    do while( iter%next( ) )
      call species_set%get( iter, species, my_name )
      call species%add( "name", species_set%key( iter ), my_name )
      new_obj%species_( i_species ) = species_t( species )
      i_species = i_species + 1
    end do
    call config%get( "shortwave lookup tables", spectrum, my_name )
    new_obj%shortwave_lookup_ = mode_shortwave_optics_lookup_t( spectrum )
    call config%get( "longwave lookup tables",  spectrum, my_name )
    new_obj%longwave_lookup_  = mode_longwave_optics_lookup_t(  spectrum )
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new state object for the mode
  function get_new_state( this ) result( new_state )

    use ai_aerosol_state,              only : aerosol_state_t

    class(aerosol_state_t), pointer    :: new_state
    class(mode_t),          intent(in) :: this

    allocate( mode_state_t :: new_state )
    select type( new_state )
    type is( mode_state_t )
       new_state%number_of_species_ = size( this%species_ )
    end select

  end function get_new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Provides an accessor for the specified optics
  function optics_accessor( this, optics )

    use ai_accessor,                   only : accessor_t
    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use mam_optics_accessor,           only : optics_accessor_t
    use mam_optics_constants,          only : shortwave_lower, longwave_lower,&
                                              shortwave_upper, longwave_upper

    class(accessor_t), pointer    :: optics_accessor
    class(mode_t),     intent(in) :: this
    class(optics_t),   intent(in) :: optics

    type(wavelength_grid_t) :: shortwave, longwave

    shortwave = wavelength_grid_t( lower_bounds = shortwave_lower,           &
                                   upper_bounds = shortwave_upper,           &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    longwave  = wavelength_grid_t( lower_bounds = longwave_lower,            &
                                   upper_bounds = longwave_upper,            &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    optics_accessor => optics_accessor_t( shortwave, longwave, optics )

  end function optics_accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a set of optical properties for the mode on a specified grid
  subroutine get_optics( this, optics_accessor, environmental_state,   &
      aerosol_state, optics )

    use ai_accessor,                   only : accessor_t
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use ai_util,                       only : die_msg
    use mam_optics_accessor,           only : optics_accessor_t

    class(mode_t),                intent(in)    :: this
    class(accessor_t),            intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics

    select type( optics_accessor )
    class is( optics_accessor_t )
      select type( aerosol_state )
      class is( mode_state_t )
        if( optics_accessor%is_shortwave( ) ) then
          call calculate_shortwave_optics( this, optics_accessor,             &
                                           environmental_state,               &
                                           aerosol_state, optics )
        end if
        if( optics_accessor%is_longwave( ) ) then
          call calculate_longwave_optics( this, optics_accessor,              &
                                          environmental_state,                &
                                          aerosol_state, optics )
        end if
      class default
        call die_msg( 623327935, "Invalid aerosol state for MAM modes" )
      end select
    class default
      call die_msg( 118121530, "Invalid accessor type for MAM mode optics" )
    end select

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs the current mode state
  subroutine print_state( this, aerosol_state, io_unit )

    use ai_util,                       only : die_msg
    use musica_string,                 only : to_char

    class(mode_t),          intent(in) :: this
    class(aerosol_state_t), intent(in) :: aerosol_state
    !> Optional output unit (defaults to 6)
    integer, optional,      intent(in) :: io_unit

    integer :: lunit, i_species

    lunit = 6
    if( present( io_unit ) ) lunit = io_unit
    select type( aerosol_state )
    class is( mode_state_t )
      write(lunit,*) "** MAM Mode State **"
      if( .not. allocated( this%species_ ) ) then
        write(lunit,*) "--- Uninitialized MAM Mode ---"
        write(lunit,*) "** End MAM Mode State **"
        return
      end if
      if( .not. associated( aerosol_state%mass_mixing_ratio__kg_kg_ ) ) then
        write(lunit,*) "--- Uninitialized MAM Mode State ---"
        write(lunit,*) "** End MAM Mode State **"
        return
      end if
      write(lunit,*) "wet mode diameter of number distribution [m]: "//       &
                     trim( to_char(                                           &
                         aerosol_state%wet_number_mode_diameter__m_ ) )
      write(lunit,*) "number mixing ratio [# mol]: "//trim( to_char(          &
                     aerosol_state%number_mixing_ratio__num_mol_ ) )
      do i_species = 1, size( this%species_ )
        write(lunit,*) "species '"//this%species_( i_species )%name( )//      &
                       "' mass mixing ratio [kg kg-1]: "//                    &
                       trim( to_char(                                         &
                       aerosol_state%mass_mixing_ratio__kg_kg_( i_species ) ) )
      end do
    class default
      call die_msg( 285756314, "Invalid state passed to MAM mode" )
    end select

  end subroutine print_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate shortwave optical properties for a given state
  subroutine calculate_shortwave_optics( this, optics_accessor,               &
      environmental_state, mode_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use mam_constants,                 only : kAccellerationByGravity
    use mam_optics_accessor,           only : optics_accessor_t
    use mam_optics_constants,  only : kNCC => kNumberOfChebyshevCoefficients, &
                                      kNSB => kNumberOfShortwaveBands
    use musica_math,                   only : chebyshev_function

    class(mode_t),                intent(in)    :: this
    class(optics_accessor_t),     intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_t),              intent(inout) :: optics

    integer                 :: i_species, i_band, i_prop
    real(kind=musica_dk)    :: absorption_coefficients( kNCC, kNSB )
    real(kind=musica_dk)    :: extinction_coefficients( kNCC, kNSB )
    real(kind=musica_dk)    :: asymmetry_parameter_coefficients( kNCC, kNSB )
    real(kind=musica_dk)    :: size_function( kNCC )
    complex(kind=musica_dk) :: net_refractive_index( kNSB )
    complex(kind=musica_dk) :: species_refractive_index
    real(kind=musica_dk)    :: species_volume_to_mass_mr ! [m3 kg-1]
    real(kind=musica_dk)    :: total_volume_to_mass_mr   ! [m3 kg-1]
    real(kind=musica_dk)    :: absorption( kNSB )
    real(kind=musica_dk)    :: extinction( kNSB )
    real(kind=musica_dk)    :: asymmetry_parameter( kNSB )
    real(kind=musica_dk)    :: normalized_radius
    real(kind=musica_dk)    :: ssa( kNSB )       ! single scattering albedo
    reaL(kind=musica_dk)    :: layer_aod( kNSB ) ! layer aerosol optical depth

    net_refractive_index    = ( 0.0_musica_dk, 0.0_musica_dk )
    total_volume_to_mass_mr = 0.0_musica_dk
    optics%values_(:,:)     = 0.0_musica_dk
    do i_species = 1, size( this%species_ )
    associate( species => this%species_( i_species ),                         &
               species_mmr =>                                                 &
                   mode_state%mass_mixing_ratio__kg_kg_( i_species ) )
      species_volume_to_mass_mr = species%volume__m3( species_mmr )
      do i_band = 1, kNSB
        species_refractive_index = species%shortwave_refractive_index( i_band )
        net_refractive_index( i_band ) = net_refractive_index( i_band ) +     &
                                         species_refractive_index *           &
                                         species_volume_to_mass_mr
      end do
      total_volume_to_mass_mr = total_volume_to_mass_mr                       &
                                + species_volume_to_mass_mr
    end associate
    end do
    net_refractive_index(:) = net_refractive_index(:)                         &
                           / max( total_volume_to_mass_mr, 1.0e-60_musica_dk )
    ! lookup Chebychen coefficients for optical properties
    call this%shortwave_lookup_%get_optics( net_refractive_index,             &
                      absorption = absorption_coefficients,                   &
                      extinction = extinction_coefficients,                   &
                      asymmetry_parameter = asymmetry_parameter_coefficients )
    ! get Chebyshev function for normalized wet number mode diameter
    normalized_radius = this%shortwave_lookup_%normalize_radius(              &
                              this%wet_surface_mode_radius__m( mode_state ) )
    call chebyshev_function( normalized_radius, size_function )
    ! calculate optical properties
    extinction = this%specific_extinction__m2_kg( mode_state, kNSB, kNCC,     &
                                                  extinction_coefficients,    &
                                                  size_function )
    absorption = this%specific_absorption__m2_kg( mode_state, kNSB, kNCC,     &
                                                 absorption_coefficients,     &
                                                 size_function,               &
                                                 max_absorption = extinction )
    absorption(:) = min( absorption(:), extinction(:) )
    asymmetry_parameter =                                                     &
        this%specific_extinction__m2_kg( mode_state, kNSB, kNCC,              &
                                         asymmetry_parameter_coefficients,    &
                                         size_function )
    ssa(:) = 1.0_musica_dk - absorption(:)                                    &
             / ( max( extinction(:), 1.0e-40_musica_dk ) )
    layer_aod = extinction(:) * environmental_state%layer_thickness__Pa( )    &
                / kAccellerationByGravity
    i_prop = optics_accessor%layer_extinction_optical_depth_index( )
    if( i_prop .gt. 0 ) optics%values_(:,i_prop) = layer_aod(:)
    i_prop = optics_accessor%layer_single_scatter_albedo_index( )
    if( i_prop .gt. 0 ) optics%values_(:,i_prop) = layer_aod(:) * ssa(:)
    i_prop = optics_accessor%asymmetry_factor_index( )
    if( i_prop .gt. 0 ) optics%values_(:,i_prop) = layer_aod(:) * ssa(:)      &
                                                   * asymmetry_parameter(:)
    i_prop = optics_accessor%forward_scattered_fraction_index( )
    if( i_prop .gt. 0 ) optics%values_(:,i_prop) = layer_aod(:) * ssa(:)      &
                                                   * asymmetry_parameter(:)   &
                                                   * asymmetry_parameter(:)

  end subroutine calculate_shortwave_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate longwave optical properties for a given state
  subroutine calculate_longwave_optics( this, optics_accessor,                &
      environmental_state, mode_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use mam_constants,                 only : kAccellerationByGravity
    use mam_optics_accessor,           only : optics_accessor_t
    use mam_optics_constants,  only : kNCC => kNumberOfChebyshevCoefficients, &
                                      kNLB => kNumberOfLongwaveBands
    use musica_math,                   only : chebyshev_function

    class(mode_t),                intent(in)    :: this
    class(optics_accessor_t),     intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_t),              intent(inout) :: optics

    integer                 :: i_species, i_band, i_prop
    real(kind=musica_dk)    :: chebyshev_coefficients( kNCC )
    real(kind=musica_dk)    :: size_function( kNCC )
    complex(kind=musica_dk) :: net_refractive_index( kNLB )
    complex(kind=musica_dk) :: species_refractive_index
    real(kind=musica_dk)    :: normalized_radius
    real(kind=musica_dk)    :: species_volume_to_mass_mr ! ![m3 kg-1]
    real(kind=musica_dk)    :: total_volume_to_mass_mr   ! ![m3 kg-1]
    real(kind=musica_dk)    :: absorption_coefficients( kNCC, kNLB )
    real(kind=musica_dk)    :: absorption( kNLB )
    real(kind=musica_dk)    :: layer_aod( kNLB )

    net_refractive_index    = ( 0.0_musica_dk, 0.0_musica_dk )
    total_volume_to_mass_mr = 0.0_musica_dk
    optics%values_(:,:)     = 0.0_musica_dk
    do i_species = 1, size( this%species_ )
    associate( species => this%species_( i_species ),                         &
               species_mmr =>                                                 &
                   mode_state%mass_mixing_ratio__kg_kg_( i_species ) )
      species_volume_to_mass_mr = species%volume__m3( species_mmr )
      do i_band = 1, kNLB
        species_refractive_index = species%longwave_refractive_index( i_band )
        net_refractive_index( i_band ) = net_refractive_index( i_band ) +     &
                                         species_refractive_index *           &
                                         species_volume_to_mass_mr
      end do
      total_volume_to_mass_mr = total_volume_to_mass_mr                       &
                                + species_volume_to_mass_mr
    end associate
    end do
    net_refractive_index(:) = net_refractive_index(:)                         &
                           / max( total_volume_to_mass_mr, 1.0e-60_musica_dk )
    ! lookup Chebychen coefficients for optical properties
    call this%longwave_lookup_%get_optics( net_refractive_index,              &
                                        absorption = absorption_coefficients )
    ! get Chebyshev function for normalized wet number mode diameter
    normalized_radius = this%shortwave_lookup_%normalize_radius(              &
                              this%wet_surface_mode_radius__m( mode_state ) )
    call chebyshev_function( normalized_radius, size_function )
    ! calculate optical properties
    absorption = this%specific_absorption__m2_kg( mode_state, kNLB, kNCC,     &
                                                  absorption_coefficients,    &
                                                  size_function )
    layer_aod(:) = absorption(:) * environmental_state%layer_thickness__Pa( ) &
                   / kAccellerationByGravity
    i_prop = optics_accessor%layer_absorption_optical_depth_index( )
    if( i_prop .gt. 0 ) optics%values_(:,i_prop) = layer_aod(:)

  end subroutine calculate_longwave_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the geometric mean diameter [m] of the number distribution of the
  !! mode
  elemental function geometric_mean_diameter_of_number_distribution__m( this, &
      mode_state ) result( gmd )

    real(kind=musica_dk)            :: gmd
    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    gmd = this%geometric_mean_diameter__m_

  end function geometric_mean_diameter_of_number_distribution__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the geometric standard deviation of the number distribution of
  !! the mode
  elemental function geometric_standard_deviation_of_number_distribution(     &
      this, mode_state ) result( gsd )

    real(kind=musica_dk)            :: gsd
    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    gsd = this%geometric_standard_deviation_

  end function geometric_standard_deviation_of_number_distribution

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the wet volume to mass mixing ratio for the mode [m3 kg_dryair-1]
  elemental function wet_volume_to_mass_mixing_ratio__m3_kg( this,            &
      mode_state ) result( vmmr )

    real(kind=musica_dk)            :: vmmr
    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    integer :: i_species

    vmmr = 0.0
    do i_species = 1, size( this%species_ )
    associate( species => this%species_( i_species ),                         &
               species_mmr =>                                                 &
                   mode_state%mass_mixing_ratio__kg_kg_( i_species ) )
      vmmr = vmmr + species%volume__m3( species_mmr )
    end associate
    end do

  end function wet_volume_to_mass_mixing_ratio__m3_kg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the mode radius [m] of the number distribution for the mode
  real(kind=musica_dk) elemental function wet_number_mode_radius__m( this,    &
      mode_state ) result( radius )

    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    radius = mode_state%wet_number_mode_diameter__m_ * 0.5_musica_dk

  end function wet_number_mode_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the mode diameter [m] of the number distribution for the mode
  real(kind=musica_dk) elemental function wet_number_mode_diameter__m( this,  &
      mode_state ) result( diameter )

    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    diameter = mode_state%wet_number_mode_diameter__m_

  end function wet_number_mode_diameter__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the mode diameter [m] of the surface area distribution for the
  !! mode
  real(kind=musica_dk) elemental function wet_surface_mode_diameter__m( this, &
      mode_state ) result( radius )

    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    radius = 2.0_musica_dk * this%wet_surface_mode_radius__m( mode_state )

  end function wet_surface_mode_diameter__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the mode radius [m] of the surface area distribution for the mode
  real(kind=musica_dk) elemental function wet_surface_mode_radius__m( this,   &
      mode_state ) result( radius )

    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    associate( sigma =>                                                       &
               this%geometric_standard_deviation_of_number_distribution(      &
                                                                mode_state ) )
      radius = this%wet_number_mode_radius__m( mode_state ) *                 &
               exp( 2.0_musica_dk * ( log( sigma )**2 ) )
    end associate

  end function wet_surface_mode_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the particle number mixing ratio per mole air [# mol-1]
  !! \todo is this per mole dry or wet air?
  real(kind=musica_dk) elemental function number_mixing_ratio__num_mol( this, &
      mode_state ) result( mixing_ratio )

    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state

    mixing_ratio = mode_state%number_mixing_ratio__num_mol_

  end function number_mixing_ratio__num_mol

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the specific absorption [m2 kg-1] for a given Chebyshev
  !! function
  pure function specific_absorption__m2_kg( this, mode_state, number_of_bands,&
      number_of_coefficients, coefficients, size_function, max_absorption )

    use mam_constants,                 only : kWaterDensitySTP
    use musica_math,                   only : weighted_chebyshev

    real(kind=musica_dk) :: specific_absorption__m2_kg( number_of_bands )
    class(mode_t),        intent(in) :: this
    class(mode_state_t),  intent(in) :: mode_state
    integer,              intent(in) :: number_of_bands
    integer,              intent(in) :: number_of_coefficients
    !> Chebyshev coefficients for absorption calculation
    real(kind=musica_dk), intent(in) :: coefficients( number_of_coefficients, &
                                                      number_of_bands )
    !> Chebyshev function for the current surface mode radius
    real(kind=musica_dk), intent(in) :: size_function( number_of_coefficients )
    !> Maximum values for specific absorption for each band [m2 kg-1]
    real(kind=musica_dk), intent(in), optional :: max_absorption(             &
                                                             number_of_bands )

    integer :: i_band
    do i_band = 1, number_of_bands
    associate( absorp => specific_absorption__m2_kg( i_band ) )
      absorp = weighted_chebyshev( number_of_coefficients,                    &
                                   coefficients( :, i_band ),                 &
                                   size_function )
      absorp = absorp *                                                       &
               this%wet_volume_to_mass_mixing_ratio__m3_kg( mode_state ) *    &
               kWaterDensitySTP
      absorp = max( 0.0_musica_dk, absorp )
      if( present( max_absorption ) ) then
        absorp = min( max_absorption( i_band ), absorp )
      end if
    end associate
    end do

  end function specific_absorption__m2_kg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the specific extinction [m2 kg-1] for a given Chebyshev
  !! function
  pure function specific_extinction__m2_kg( this, mode_state, number_of_bands,&
      number_of_coefficients, coefficients, size_function )

    use mam_constants,                 only : kWaterDensitySTP
    use musica_math,                   only : weighted_chebyshev

    real(kind=musica_dk) :: specific_extinction__m2_kg( number_of_bands )
    class(mode_t),        intent(in) :: this
    class(mode_state_t),  intent(in) :: mode_state
    integer,              intent(in) :: number_of_bands
    integer,              intent(in) :: number_of_coefficients
    !> Chebyshev coefficients for extinction calculation
    real(kind=musica_dk), intent(in) :: coefficients( number_of_coefficients, &
                                                      number_of_bands )
    !> Chebyshev function for the current surface mode radius
    real(kind=musica_dk), intent(in) :: size_function( number_of_coefficients )

    integer :: i_band
    real(kind=musica_dk) :: surf_rad

    surf_rad = this%wet_surface_mode_radius__m( mode_state )
    if( surf_rad .le. this%shortwave_lookup_%maximum_radius__m( ) ) then
      do i_band = 1, number_of_bands
      associate( ext => specific_extinction__m2_kg( i_band ) )
        ext = weighted_chebyshev( number_of_coefficients,                     &
                                  coefficients( :, i_band ),                  &
                                  size_function )
        !> \todo is the data returned from the lookup table in ln( m2/kg )?
        ext = exp( ext )
      end associate
      end do
    else
      specific_extinction__m2_kg(:) = 1.5_musica_dk                           &
                                      / ( surf_rad * kWaterDensitySTP )
    end if
    !> \todo in the original code, this is labelled as converting "m2/kg
    !! water to m2/kg aerosol", but it appears to convert instead to
    !! m2/kg dry air - is this right???
    specific_extinction__m2_kg(:) = specific_extinction__m2_kg(:)             &
                  * this%wet_volume_to_mass_mixing_ratio__m3_kg( mode_state ) &
                  * kWaterDensitySTP

  end function specific_extinction__m2_kg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the asymmetry parameter [unitless ratio] for a given Chebyshev
  !! function
  pure function asymmetry_parameter( this, mode_state, number_of_bands,       &
      number_of_coefficients, coefficients, size_function )

    use mam_constants,                 only : kWaterDensitySTP
    use musica_math,                   only : weighted_chebyshev

    real(kind=musica_dk) :: asymmetry_parameter( number_of_bands )
    class(mode_t),       intent(in)  :: this
    class(mode_state_t), intent(in)  :: mode_state
    integer,              intent(in) :: number_of_bands
    integer,             intent(in)  :: number_of_coefficients
    !> Chebyshev coefficients for asymmetry parameter calculation
    real(kind=musica_dk), intent(in) :: coefficients( number_of_coefficients, &
                                                       number_of_bands )
    !> Chebyshev function for the current surface mode radius
    real(kind=musica_dk), intent(in) :: size_function( number_of_coefficients )

    integer :: i_band

    do i_band = 1, number_of_bands
    associate( asym => asymmetry_parameter )
      asym = weighted_chebyshev( number_of_coefficients,                      &
                                 coefficients( :, i_band ),                   &
                                 size_function )
    end associate
    end do

  end function asymmetry_parameter

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! mode_state_t functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of doubles needed to hold the raw mode state
  integer function raw_size( this )

    class(mode_state_t), intent(in) :: this

    raw_size = 2 + this%number_of_species_

  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads raw mode state data to the mode_state_t object
  subroutine load_state( this, raw_state, index )

    class(mode_state_t),          intent(inout) :: this
    real(kind=musica_dk), target, intent(inout) :: raw_state(:)
    integer, optional,            intent(inout) :: index

    integer :: id, last_id

    id = 1
    if( present( index ) ) id = index
    this%wet_number_mode_diameter__m_ => raw_state( id )
    this%number_mixing_ratio__num_mol_ => raw_state( id + 1 )
    id = id + 2
    last_id = id + this%number_of_species_ - 1
    this%mass_mixing_ratio__kg_kg_ => raw_state( id : last_id )
    if( present( index ) ) index = last_id + 1

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Nullifies the raw mode state pointers
  subroutine dump_state( this, raw_state, index )

    class(mode_state_t),  intent(inout) :: this
    real(kind=musica_dk), intent(inout) :: raw_state(:)
    integer, optional,    intent(inout) :: index

    this%wet_number_mode_diameter__m_ => null( )
    this%number_mixing_ratio__num_mol_ => null( )
    this%mass_mixing_ratio__kg_kg_ => null( )
    if( present( index ) ) index = index + 2 + this%number_of_species_

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Set the mode state to a random, but reasonable, state. For testing only.
  subroutine randomize( this )

    use musica_assert,                 only : assert_msg

    class(mode_state_t), intent(inout) :: this

    real(kind=musica_dk) :: rand_val
    integer              :: i_species

    !> \todo Make sure random mode state is reasonable
    call assert_msg( 175218277,                                               &
                     associated( this%wet_number_mode_diameter__m_ ) .and.    &
                     associated( this%mass_mixing_ratio__kg_kg_ ),            &
                     "Trying to randomize an unassociated mode state" )
    call random_number( rand_val )
    this%wet_number_mode_diameter__m_ = 10.0**( rand_val * 3 - 5 )
    this%number_mixing_ratio__num_mol_ = 10.0**( rand_val * 6 )
    do i_species = 1, size( this%mass_mixing_ratio__kg_kg_ )
      call random_number( rand_val )
      this%mass_mixing_ratio__kg_kg_( i_species ) = rand_val * 2.0e-9
    end do

  end subroutine randomize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_mode
