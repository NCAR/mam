! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_mode module

!> The mode_t type and related functions
module mam_mode

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use mam_optics_lookup,               only : optics_lookup_t
  use mam_species,                     only : species_t
  use musica_constants,                only : musica_dk

  implicit none
  private

  public :: mode_t, mode_state_t

  !> An aerosol mode
  type, extends(aerosol_t) :: mode_t
    private
    !> Geometric mean diameter [m] of number distribution
    real(kind=musica_dk)         :: geometric_mean_diameter__m_
    !> Geometric standard deviation of number distribution
    real(kind=musica_dk)         :: geometric_standard_deviation_
    !> Chemical species that can be present in the mode
    type(species_t), allocatable :: species_(:)
    !> Shortwave optical property lookup table
    type(optics_lookup_t)        :: shortwave_lookup_
    !> Longwave optical property lookup table
    type(optics_lookup_t)        :: longwave_lookup_
  contains
    procedure :: new_state
    procedure :: new_optics
    procedure, private :: shortwave_optics_scalar
    procedure, private :: shortwave_optics_array
    procedure, private :: longwave_optics_scalar
    procedure, private :: longwave_optics_array
    procedure :: shortwave_grid
    procedure :: longwave_grid
    procedure :: print_state
    procedure :: add_shortwave_optics
    procedure :: add_longwave_optics
    procedure :: geometric_mean_diameter_of_number_distribution__m
    procedure :: geometric_standard_deviation_of_number_distribution
    procedure :: wet_volume_to_mass_mixing_ratio__m3_kg
    procedure :: wet_number_mode_radius__m
    procedure :: wet_number_mode_diameter__m
    procedure :: wet_surface_mode_radius__m
    procedure :: wet_surface_mode_diameter__m
    procedure :: number_mixing_ratio__num_mol
    procedure :: net_shortwave_refractive_index
    procedure :: net_longwave_refractive_index
    procedure :: specific_absorption__m2_kg
    procedure :: specific_extinction__m2_kg
    procedure :: asymmetry_factor
  end type mode_t

  interface mode_t
    module procedure :: constructor
  end interface mode_t

  !> Aerosol mode state
  type, extends(aerosol_state_t) :: mode_state_t
    private
    !> Mode diameter of the wet aerosol number distribution [m]
    real(kind=musica_dk)              :: wet_number_mode_diameter__m_
    !> Particle number mixing ratio per mol_air [# mol-1]
    !! \todo Is this dry air or wet air?
    real(kind=musica_dk)              :: number_mixing_ratio__num_mol_
    !> Mass mixing ratio of each mode species relative to dry air [kg kg-1]
    real(kind=musica_dk), allocatable :: mass_mixing_ratio__kg_kg_(:)
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
    type(config_t) :: optics_config, species_set, species
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
    call config%get( "shortwave lookup tables", optics_config, my_name )
    new_obj%shortwave_lookup_ = optics_lookup_t( optics_config )
    call config%get( "longwave lookup tables",  optics_config, my_name )
    new_obj%longwave_lookup_  = optics_lookup_t( optics_config )
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new state object for the mode
  function new_state( this )

    use ai_aerosol_state,              only : aerosol_state_t

    class(aerosol_state_t), pointer    :: new_state
    class(mode_t),          intent(in) :: this

    allocate( mode_state_t :: new_state )
    select type( new_state )
    type is( mode_state_t )
       allocate( new_state%mass_mixing_ratio__kg_kg_( size( this%species_ ) ) )
    end select

  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates an optics_t object for a given property
  function new_optics( this, property, output_grid, interpolation_strategy )

    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t
    use mam_optics_util,               only : create_optics
    use musica_interpolator,           only : interpolation_strategy_i
    use musica_property,               only : property_t

    class(optics_t),         pointer    :: new_optics
    class(mode_t),           intent(in) :: this
    class(property_t),       intent(in) :: property
    type(wavelength_grid_t), intent(in) :: output_grid
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

    new_optics =>                                                             &
        create_optics( property, this%shortwave_lookup_%grid( ),              &
                       this%longwave_lookup_%grid( ), output_grid,            &
                       interpolation_strategy )

  end function new_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_scalar( this, environmental_state,              &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%shortwave_optics( environmental_state, aerosol_state,           &
                                optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine shortwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_array( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die_msg

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_prop

    select type( aerosol_state )
    class is( mode_state_t )
      do i_prop = 1, size( optics )
        call optics( i_prop )%ptr_%reset_values( )
      end do
      call this%add_shortwave_optics( environmental_state, aerosol_state,     &
                                      optics )
    class default
      call die_msg( 713383720, "Invalid state passed to MAM mode shortwave "//&
                               "optics calculator" )
    end select

  end subroutine shortwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_scalar( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%longwave_optics( environmental_state, aerosol_state,            &
                                optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine longwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_array( this, environmental_state,                &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die_msg

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_prop

    select type( aerosol_state )
    class is( mode_state_t )
      do i_prop = 1, size( optics )
        call optics( i_prop )%ptr_%reset_values( )
      end do
      call this%add_longwave_optics( environmental_state, aerosol_state,      &
                                      optics )
    class default
      call die_msg( 713383720, "Invalid state passed to MAM mode longwave "// &
                               "optics calculator" )
    end select

  end subroutine longwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the native shortwave optics wavelength grid
  function shortwave_grid( this )

    use ai_wavelength_grid,            only : wavelength_grid_t

    type(wavelength_grid_t)   :: shortwave_grid
    class(mode_t), intent(in) :: this

    shortwave_grid = this%shortwave_lookup_%grid( )

  end function shortwave_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the native longwave optics wavelength grid
  function longwave_grid( this )

    use ai_wavelength_grid,            only : wavelength_grid_t

    type(wavelength_grid_t)   :: longwave_grid
    class(mode_t), intent(in) :: this

    longwave_grid = this%longwave_lookup_%grid( )

  end function longwave_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs the current mode state
  subroutine print_state( this, aerosol_state, io_unit )

    use musica_assert,                 only : die_msg
    use musica_string,                 only : to_char

    !> MAM mode
    class(mode_t),          intent(in) :: this
    !> MAM mode state
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
      if( .not. allocated( aerosol_state%mass_mixing_ratio__kg_kg_ ) ) then
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

  !> Calculates shortwave optical properties for a given state and adds them
  !! to the set of optical properties
  subroutine add_shortwave_optics( this, environmental_state, mode_state,     &
      optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use mam_constants,                 only : kAccellerationByGravity
    use mam_optics_util,               only : add_shortwave_property
    use musica_math,                   only : chebyshev_function

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_ptr),            intent(inout) :: optics(:)

    integer                 :: i_prop, n_cheby, n_bands
    real(kind=musica_dk),    allocatable :: absorption_coefficients(:,:)
    real(kind=musica_dk),    allocatable :: extinction_coefficients(:,:)
    real(kind=musica_dk),    allocatable :: asymmetry_factor_coefficients(:,:)
    real(kind=musica_dk),    allocatable :: size_function(:)
    complex(kind=musica_dk), allocatable :: net_refractive_index(:)
    real(kind=musica_dk),    allocatable :: absorption(:)
    real(kind=musica_dk),    allocatable :: extinction(:)
    real(kind=musica_dk),    allocatable :: asymmetry_factor(:)
    real(kind=musica_dk),    allocatable :: ssa(:)       ! single scattering albedo
    reaL(kind=musica_dk),    allocatable :: layer_aod(:) ! layer aerosol optical depth
    real(kind=musica_dk)                 :: normalized_radius

    n_cheby = this%shortwave_lookup_%number_of_chebyshev_coefficients( )
    n_bands = this%shortwave_lookup_%number_of_wavelength_bands( )
    allocate( absorption_coefficients(       n_cheby, n_bands ) )
    allocate( extinction_coefficients(       n_cheby, n_bands ) )
    allocate( asymmetry_factor_coefficients( n_cheby, n_bands ) )
    allocate( size_function(        n_cheby ) )
    allocate( net_refractive_index( n_bands ) )
    allocate( absorption(           n_bands ) )
    allocate( extinction(           n_bands ) )
    allocate( asymmetry_factor(     n_bands ) )
    allocate( ssa(                  n_bands ) )
    allocate( layer_aod(            n_bands ) )
    net_refractive_index =                                                    &
        this%net_shortwave_refractive_index( mode_state, n_bands )
    ! lookup Chebyshev coefficients for optical properties
    call this%shortwave_lookup_%get_optics( net_refractive_index,             &
                      absorption = absorption_coefficients,                   &
                      extinction = extinction_coefficients,                   &
                      asymmetry_factor = asymmetry_factor_coefficients )
    ! get Chebyshev function for normalized wet number mode diameter
    normalized_radius = this%shortwave_lookup_%normalize_radius(              &
                              this%wet_surface_mode_radius__m( mode_state ) )
    call chebyshev_function( normalized_radius, size_function )
    ! calculate optical properties
    extinction = this%specific_extinction__m2_kg( mode_state, n_bands,        &
                                                  n_cheby,                    &
                                                  extinction_coefficients,    &
                                                  size_function,              &
                                                  this%shortwave_lookup_ )
    absorption = this%specific_absorption__m2_kg( mode_state, n_bands,        &
                                                 n_cheby,                     &
                                                 absorption_coefficients,     &
                                                 size_function,               &
                                                 max_absorption = extinction )
    absorption(:) = min( absorption(:), extinction(:) )
    asymmetry_factor = this%asymmetry_factor( mode_state, n_bands, n_cheby,   &
                                              asymmetry_factor_coefficients,  &
                                             size_function )
    ssa(:) = 1.0_musica_dk - absorption(:)                                    &
             / ( max( extinction(:), 1.0e-40_musica_dk ) )
    layer_aod = extinction(:) * environmental_state%layer_thickness__Pa( )    &
                / kAccellerationByGravity
    do i_prop = 1, size( optics )
      call add_shortwave_property( layer_aod, ssa, asymmetry_factor,          &
                                   optics( i_prop )%ptr_ )
    end do

  end subroutine add_shortwave_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates longwave optical properties for a given state and adds them
  !!! to the set of optical properties
  subroutine add_longwave_optics( this, environmental_state, mode_state,      &
      optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use mam_constants,                 only : kAccellerationByGravity
    use mam_optics_util,               only : add_longwave_property
    use musica_math,                   only : chebyshev_function

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_ptr),            intent(inout) :: optics(:)

    integer                 :: i_prop, n_cheby, n_bands
    real(kind=musica_dk),    allocatable :: absorption_coefficients(:,:)
    real(kind=musica_dk),    allocatable :: chebyshev_coefficients(:)
    real(kind=musica_dk),    allocatable :: size_function(:)
    complex(kind=musica_dk), allocatable :: net_refractive_index(:)
    real(kind=musica_dk),    allocatable :: absorption(:)
    real(kind=musica_dk),    allocatable :: layer_aod(:)
    real(kind=musica_dk)    :: normalized_radius

    n_cheby = this%longwave_lookup_%number_of_chebyshev_coefficients( )
    n_bands = this%longwave_lookup_%number_of_wavelength_bands( )
    allocate( absorption_coefficients( n_cheby, n_bands ) )
    allocate( chebyshev_coefficients(  n_cheby ) )
    allocate( size_function(           n_cheby ) )
    allocate( net_refractive_index(    n_bands ) )
    allocate( absorption(              n_bands ) )
    allocate( layer_aod(               n_bands ) )
    net_refractive_index =                                                    &
        this%net_longwave_refractive_index( mode_state, n_bands )
    ! lookup Chebyshev coefficients for optical properties
    call this%longwave_lookup_%get_optics( net_refractive_index,              &
                                        absorption = absorption_coefficients )
    ! get Chebyshev function for normalized wet number mode diameter
    normalized_radius = this%longwave_lookup_%normalize_radius(               &
                              this%wet_surface_mode_radius__m( mode_state ) )
    call chebyshev_function( normalized_radius, size_function )
    ! calculate optical properties
    absorption = this%specific_absorption__m2_kg( mode_state, n_bands,        &
                                                  n_cheby,                    &
                                                  absorption_coefficients,    &
                                                  size_function )
    layer_aod(:) = absorption(:) * environmental_state%layer_thickness__Pa( ) &
                   / kAccellerationByGravity
    do i_prop = 1, size( optics )
      call add_longwave_property( layer_aod, optics( i_prop )%ptr_ )
    end do

  end subroutine add_longwave_optics

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

  !> Calculates the net refractive index for the mode based on weighted
  !! contributions from constituent species in the shortwave region
  pure function net_shortwave_refractive_index( this, mode_state,             &
      number_of_bands ) result( refractive_index )

    complex(kind=musica_dk)         :: refractive_index( number_of_bands )
    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: number_of_bands

    integer              :: i_species, i_band
    real(kind=musica_dk) :: total_vmmr, species_vmmr

    refractive_index(:) = ( 0.0_musica_dk, 0.0_musica_dk )
    total_vmmr = 0.0_musica_dk
    do i_species = 1, size( this%species_ )
    associate( species     => this%species_( i_species ),                     &
               species_mmr =>                                                 &
                   mode_state%mass_mixing_ratio__kg_kg_( i_species ) )
      species_vmmr = species%volume__m3( species_mmr )
      do i_band = 1, number_of_bands
        refractive_index( i_band ) = refractive_index( i_band )               &
                 + species%shortwave_refractive_index( i_band ) * species_vmmr
      end do
      total_vmmr = total_vmmr + species_vmmr
    end associate
    end do
    refractive_index(:) = refractive_index(:)                                 &
                          / max( total_vmmr, 1.0e-60_musica_dk )

  end function net_shortwave_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the net refractive index for the mode based on weighted
  !! contributions from constituent species in the longwave region
  pure function net_longwave_refractive_index( this, mode_state,             &
      number_of_bands ) result( refractive_index )

    complex(kind=musica_dk)         :: refractive_index( number_of_bands )
    class(mode_t),       intent(in) :: this
    class(mode_state_t), intent(in) :: mode_state
    integer,             intent(in) :: number_of_bands

    integer              :: i_species, i_band
    real(kind=musica_dk) :: total_vmmr, species_vmmr

    refractive_index(:) = ( 0.0_musica_dk, 0.0_musica_dk )
    total_vmmr = 0.0_musica_dk
    do i_species = 1, size( this%species_ )
    associate( species     => this%species_( i_species ),                     &
               species_mmr =>                                                 &
                   mode_state%mass_mixing_ratio__kg_kg_( i_species ) )
      species_vmmr = species%volume__m3( species_mmr )
      do i_band = 1, number_of_bands
        refractive_index( i_band ) = refractive_index( i_band )               &
                 + species%longwave_refractive_index( i_band ) * species_vmmr
      end do
      total_vmmr = total_vmmr + species_vmmr
    end associate
    end do
    refractive_index(:) = refractive_index(:)                                 &
                          / max( total_vmmr, 1.0e-60_musica_dk )

  end function net_longwave_refractive_index

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
      number_of_coefficients, coefficients, size_function, optics_lookup )

    use mam_constants,                 only : kWaterDensitySTP
    use musica_math,                   only : weighted_chebyshev

    !> Specific extinction by wavelength
    real(kind=musica_dk) :: specific_extinction__m2_kg( number_of_bands )
    !> MAM mode
    class(mode_t),        intent(in) :: this
    !> MAM mode state
    class(mode_state_t),  intent(in) :: mode_state
    !> Number of wavelength bands
    integer,              intent(in) :: number_of_bands
    !> Number of Chebyshev coefficients
    integer,              intent(in) :: number_of_coefficients
    !> Chebyshev coefficients for extinction calculation
    real(kind=musica_dk), intent(in) :: coefficients( number_of_coefficients, &
                                                      number_of_bands )
    !> Chebyshev function for the current surface mode radius
    real(kind=musica_dk), intent(in) :: size_function( number_of_coefficients )
    !> Optics lookup table
    class(optics_lookup_t), intent(in) :: optics_lookup

    integer :: i_band
    real(kind=musica_dk) :: surf_rad

    surf_rad = this%wet_surface_mode_radius__m( mode_state )
    if( surf_rad .le. optics_lookup%maximum_radius__m( ) ) then
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
  pure function asymmetry_factor( this, mode_state, number_of_bands,          &
      number_of_coefficients, coefficients, size_function )

    use mam_constants,                 only : kWaterDensitySTP
    use musica_math,                   only : weighted_chebyshev

    !> Asymmetry factor by wavelength
    real(kind=musica_dk) :: asymmetry_factor( number_of_bands )
    !> MAM mode
    class(mode_t),        intent(in) :: this
    !> MAM mode state
    class(mode_state_t),  intent(in) :: mode_state
    !> Number of wavelength bands
    integer,              intent(in) :: number_of_bands
    !> Number of Chebyshev coefficients
    integer,              intent(in) :: number_of_coefficients
    !> Chebyshev coefficients for asymmetry parameter calculation
    real(kind=musica_dk), intent(in) :: coefficients( number_of_coefficients, &
                                                      number_of_bands )
    !> Chebyshev function for the current surface mode radius
    real(kind=musica_dk), intent(in) :: size_function( number_of_coefficients )

    integer :: i_band

    do i_band = 1, number_of_bands
    associate( asym => asymmetry_factor( i_band ) )
      asym = weighted_chebyshev( number_of_coefficients,                      &
                                 coefficients( :, i_band ),                   &
                                 size_function )
    end associate
    end do

  end function asymmetry_factor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! mode_state_t functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of doubles needed to hold the raw mode state
  integer function raw_size( this )

    class(mode_state_t), intent(in) :: this

    raw_size = 2 + size( this%mass_mixing_ratio__kg_kg_ )

  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads raw mode state data to the mode_state_t object
  subroutine load_state( this, raw_state, index )

    class(mode_state_t),  intent(inout) :: this
    real(kind=musica_dk), intent(in)    :: raw_state(:)
    integer, optional,    intent(inout) :: index

    integer :: id, last_id

    id = 1
    if( present( index ) ) id = index
    this%wet_number_mode_diameter__m_ = raw_state( id )
    this%number_mixing_ratio__num_mol_ = raw_state( id + 1 )
    id = id + 2
    last_id = id + size( this%mass_mixing_ratio__kg_kg_ ) - 1
    this%mass_mixing_ratio__kg_kg_(:) = raw_state( id : last_id )
    if( present( index ) ) index = last_id + 1

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the aerosol state into the raw state array
  subroutine dump_state( this, raw_state, index )

    class(mode_state_t),  intent(in)    :: this
    real(kind=musica_dk), intent(inout) :: raw_state(:)
    integer, optional,    intent(inout) :: index

    integer :: id, last_id

    id = 1
    if( present( index ) ) id = index
    raw_state( id )     = this%wet_number_mode_diameter__m_
    raw_state( id + 1 ) = this%number_mixing_ratio__num_mol_
    id = id + 2
    last_id = id + size( this%mass_mixing_ratio__kg_kg_ ) - 1
    raw_state( id : last_id ) = this%mass_mixing_ratio__kg_kg_(:)
    if( present( index ) ) index = last_id + 1

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Set the mode state to a random, but reasonable, state. For testing only.
  subroutine randomize( this )

    class(mode_state_t), intent(inout) :: this

    real(kind=musica_dk) :: rand_val
    integer              :: i_species

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
